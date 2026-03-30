// v7: hierCoverage with dynamic hash sizing + extmodule I/O proxy coverage.
// Based on v6a with three key improvements:
//
// Changes vs v6a:
//   - Dynamic hash GROWTH: hash size scales as log2(numBits) instead of fixed cap.
//     Large modules (e.g. LSU with 408 ctrl regs) get 8-10 bit hashes instead of 6.
//   - Raised maxRegBits from 64 to 256: samples more control register state.
//   - ExtModule I/O proxy: hashes input ports of extmodule children (e.g. NBDTLB stub)
//     as a proxy for their internal state, instead of contributing 0 bits.
//   - Raised maxInputHashSize/maxCoreHashSize caps, added minHashSize floor.
package hier_cov

import java.io.{File, PrintWriter}

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._
import firrtl2.PrimOps._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import coverage.graphLedger

case class HierCovParamsV7(
  maxInputHashSize: Int = 10,  // raised from 6 — cap for dynamic input hash
  maxCoreHashSize: Int = 12,   // raised from 6 — cap for dynamic core hash
  minHashSize: Int = 4,        // NEW — floor for hash width (prevents too-small maps)
  submodHashSize: Int = 6,
  covSumSize: Int = 32,
  maxInputPorts: Int = 8,
  maxBitsPerPort: Int = 8,
  maxRegBits: Int = 256,       // raised from 64 — sample more ctrl reg bits before cap
  maxCtrlRegWidth: Int = 20,   // skip control regs wider than this
  maxExtModPorts: Int = 16,    // NEW — max extmodule input ports to sample
  maxExtModBitsPerPort: Int = 8, // NEW — bits per extmodule port
  bucketCount: Int = 16,
  bucketWidth: Int = 8,
  maxBucketSigBits: Int = 128
)

object HierCovUtilV7 {
  def stableHash(s: String): Int = {
    s.foldLeft(0)((h, c) => (h * 31 + c.toInt) & 0x7fffffff)
  }

  def hasClock(mod: Module): (String, Boolean) = {
    val clockName = mod.ports.find(p => p.name == "clock" || p.name == "gated_clock" || p.name == "clk").map(_.name)
    (clockName.getOrElse("None"), clockName.isDefined)
  }

  def typeWidthOpt(tpe: Type): Option[Int] = tpe match {
    case UIntType(IntWidth(w)) => Some(w.toInt)
    case SIntType(IntWidth(w)) => Some(w.toInt)
    case _ => None
  }

  def bitExtract(expr: Expression, idx: Int, tpe: Type): Expression = {
    val width = typeWidthOpt(tpe).getOrElse(
      throw new Exception(s"Unsupported type width: ${tpe.serialize}")
    )
    val asUInt = tpe match {
      case UIntType(_) => expr
      case SIntType(_) => DoPrim(AsUInt, Seq(expr), Seq(), UIntType(IntWidth(width)))
      case _ => throw new Exception(s"Unsupported type for bitExtract: ${tpe.serialize}")
    }
    DoPrim(Bits, Seq(asUInt), Seq(idx, idx), UIntType(IntWidth(1)))
  }

  def xorReduce(bits: Seq[Expression]): Expression = {
    if (bits.isEmpty) {
      UIntLiteral(0, IntWidth(1))
    } else {
      bits.reduce((a, b) => DoPrim(Xor, Seq(a, b), Seq(), UIntType(IntWidth(1))))
    }
  }

  def catBits(bits: Seq[Expression]): Expression = {
    if (bits.isEmpty) {
      UIntLiteral(0, IntWidth(1))
    } else if (bits.length == 1) {
      bits.head
    } else {
      var acc = bits.head
      var accWidth = 1
      for (b <- bits.tail) {
        accWidth = accWidth + 1
        acc = DoPrim(Cat, Seq(acc, b), Seq(), UIntType(IntWidth(accWidth)))
      }
      acc
    }
  }

  def log2Ceil(x: Int): Int = {
    var v = x - 1
    var r = 0
    while (v > 0) {
      v = v >> 1
      r = r + 1
    }
    r
  }

  // Select bits from DATA input ports — ports that do NOT feed mux selects.
  // Excludes control ports (those in ctrlPortNames) to avoid mixing control/data signals.
  def selectDataInputBits(ports: Seq[Port], ctrlPortNames: Set[String], params: HierCovParamsV7): Seq[(Expression, String)] = {
    val dataInputs = ports
      .filter(p => p.direction == Input)
      .filterNot { p =>
        val lname = p.name.toLowerCase
        p.tpe == ClockType || lname.contains("clock") || lname.contains("reset")
      }
      .filter(p => p.tpe.isInstanceOf[UIntType] || p.tpe.isInstanceOf[SIntType])
      .filterNot(p => ctrlPortNames.contains(p.name))   // exclude control inputs

    val limitedPorts = dataInputs.take(params.maxInputPorts)

    limitedPorts.flatMap { p =>
      typeWidthOpt(p.tpe).toSeq.flatMap { width =>
        val bitsToTake = Math.min(params.maxBitsPerPort, width)
        val stride = Math.max(1, width / bitsToTake)
        val bitIdxs = (0 until width by stride).take(bitsToTake)
        bitIdxs.map { idx =>
          val bit = bitExtract(WRef(p), idx, p.tpe)
          (bit, s"${p.name}[$idx]")
        }
      }
    }
  }

  // Select bits from CONTROL registers only (registers feeding mux selects).
  // Excludes dirInRegs (directly-input-fed regs, width > 3) and wide regs (>= maxCtrlRegWidth).
  def selectControlRegBits(
    ctrlRegs: scala.collection.Set[DefRegister],
    dirInRegs: scala.collection.Set[DefRegister],
    params: HierCovParamsV7
  ): Seq[(Expression, String)] = {
    val filteredDirIn = dirInRegs.filter(r => typeWidthOpt(r.tpe).getOrElse(0) > 3)
    val regs = ctrlRegs
      .filterNot(filteredDirIn.contains(_))
      .filter(r => typeWidthOpt(r.tpe).getOrElse(0) < params.maxCtrlRegWidth)
      .toSeq

    val bits = ListBuffer[(Expression, String)]()
    for (reg <- regs) {
      typeWidthOpt(reg.tpe).foreach { width =>
        val stride = Math.max(1, width / Math.min(width, 8))
        val bitIdxs = (0 until width by stride)
        for (idx <- bitIdxs) {
          bits.append((bitExtract(WRef(reg), idx, reg.tpe), s"${reg.name}[$idx]"))
          if (bits.size >= params.maxRegBits) return bits.toSeq
        }
      }
    }
    bits.toSeq
  }

  def buildHash(bits: Seq[(Expression, String)], width: Int, baseName: String): (Expression, Seq[Statement]) = {
    if (width <= 0) {
      return (UIntLiteral(0, IntWidth(1)), Seq[Statement]())
    }

    val buckets = Array.fill[ListBuffer[Expression]](width)(ListBuffer[Expression]())
    for ((expr, name) <- bits) {
      val idx = stableHash(name) % width
      buckets(idx).append(expr)
    }

    val bitWires = (0 until width).map(i => DefWire(NoInfo, s"${baseName}_b${i}", UIntType(IntWidth(1))))
    val bitCons = bitWires.zipWithIndex.map { case (w, i) =>
      val xorExpr = xorReduce(buckets(i).toSeq)
      Connect(NoInfo, WRef(w), xorExpr)
    }

    val hashWire = DefWire(NoInfo, s"${baseName}_hash", UIntType(IntWidth(width)))
    val catExpr = catBits(bitWires.reverse.map(w => WRef(w)))
    val hashCon = Connect(NoInfo, WRef(hashWire), catExpr)

    (WRef(hashWire), bitWires ++ bitCons :+ hashWire :+ hashCon)
  }

  def defMemory(name: String, info: String, size: Int, addrWidth: Int): (DefMemory, WRef) = {
    val mem = DefMemory(FileInfo(StringLit(info)), name,
      UIntType(IntWidth(1)), size, 1, 0,
      Seq("read"), Seq("write"), Seq())
    val ref = WRef(name, BundleType(Seq(
      Field("read", Flip, BundleType(List(
        Field("addr", Default, UIntType(IntWidth(addrWidth))),
        Field("en", Default, UIntType(IntWidth(1))),
        Field("clk", Default, ClockType),
        Field("data", Flip, UIntType(IntWidth(1)))
      ))),
      Field("write", Flip, BundleType(List(
        Field("addr", Default, UIntType(IntWidth(addrWidth))),
        Field("mask", Default, UIntType(IntWidth(1))),
        Field("en", Default, UIntType(IntWidth(1))),
        Field("clk", Default, ClockType),
        Field("data", Default, UIntType(IntWidth(1)))
      )))
    )), MemKind, SourceFlow)

    (mem, ref)
  }

  def xorFoldAddr(addr: Expression, addrWidth: Int, outWidth: Int): Expression = {
    if (outWidth <= 0) return UIntLiteral(0, IntWidth(1))
    if (addrWidth <= outWidth) return addr

    val chunks = (0 until addrWidth by outWidth).map { lo =>
      val hi = Math.min(addrWidth - 1, lo + outWidth - 1)
      DoPrim(Bits, Seq(addr), Seq(hi, lo), UIntType(IntWidth(hi - lo + 1)))
    }

    chunks.reduce((a, b) => DoPrim(Xor, Seq(a, b), Seq(), UIntType(IntWidth(outWidth))))
  }
}

class HierModuleInfoV7(
  val mName: String,
  val insts: scala.collection.Set[WDefInstance],
  val regs: scala.collection.Set[DefRegister],       // ALL regs — needed for metaReset
  val ctrlRegs: scala.collection.Set[DefRegister],   // control regs only
  val dirInRegs: scala.collection.Set[DefRegister],  // directly-input-fed control regs
  val ctrlPortNames: Set[String]                     // port names feeding mux selects
)

object HierModuleInfoV7 {
  def apply(mod: DefModule, gLedger: graphLedger): HierModuleInfoV7 = {
    val insts = gLedger.getInstances
    val regs = gLedger.findRegs
    // Order matters: findMuxSrcs must run before findDirInRegs (populates reverseMap + ctrlSrcs)
    val ctrlSrcs = gLedger.findMuxSrcs
    val dirInRegs = gLedger.findDirInRegs
    val ctrlRegs = ctrlSrcs("DefRegister").map(_.node.asInstanceOf[DefRegister]).toSet
    val ctrlPortNames = ctrlSrcs("Port").map(_.name).toSet
    new HierModuleInfoV7(mod.name, insts, regs, ctrlRegs, dirInRegs, ctrlPortNames)
  }
}

class InstrHierCovV7(mod: DefModule,
                      mInfo: HierModuleInfoV7,
                      extModules: Set[String],
                      extModulePorts: scala.collection.Map[String, Seq[Port]],
                      params: HierCovParamsV7) {
  import HierCovUtilV7._

  private val mName = mod.name

  def instrument(): DefModule = {
    mod match {
      case m: Module =>
        val stmts = m.body.asInstanceOf[Block].stmts
        val (clockName, hasClockFound) = hasClock(m)

        val inputBits = selectDataInputBits(m.ports, mInfo.ctrlPortNames, params)
        val regBits = selectControlRegBits(mInfo.ctrlRegs, mInfo.dirInRegs, params)

        val submodBits = mInfo.insts.filter(inst => !extModules.contains(inst.module)).flatMap { inst =>
          val ref = WSubField(WRef(inst), "io_hierCovHash")
          (0 until params.submodHashSize).map { idx =>
            val bit = bitExtract(ref, idx, UIntType(IntWidth(params.submodHashSize)))
            (bit, s"${inst.name}.io_hierCovHash[$idx]")
          }
        }.toSeq

        // Proxy coverage for extmodule children: hash their input ports since
        // we can't see inside them. Captures input-state variation as a proxy.
        val extmodBits = mInfo.insts.filter(inst => extModules.contains(inst.module)).flatMap { inst =>
          extModulePorts.getOrElse(inst.module, Seq.empty)
            .filter(p => p.direction == Input)
            .filter(p => p.tpe.isInstanceOf[UIntType] || p.tpe.isInstanceOf[SIntType])
            .filterNot { p =>
              val lname = p.name.toLowerCase
              lname.contains("clock") || lname.contains("reset") || lname.contains("metareset")
            }
            .take(params.maxExtModPorts)
            .flatMap { p =>
              typeWidthOpt(p.tpe).toSeq.flatMap { width =>
                val bitsToTake = Math.min(params.maxExtModBitsPerPort, width)
                val stride = Math.max(1, width / bitsToTake)
                (0 until width by stride).take(bitsToTake).map { idx =>
                  val bit = bitExtract(WSubField(WRef(inst), p.name), idx, p.tpe)
                  (bit, s"${inst.name}.${p.name}[$idx]")
                }
              }
            }
        }.toSeq

        val coreBitsAll = regBits ++ submodBits ++ extmodBits
        // Dynamic hash sizing: scale as log2(numBits), clamped to [minHashSize, maxHashSize].
        // For small modules (few bits): hash = numBits (no compression needed).
        // For large modules (many bits): hash = log2(numBits), growing with module complexity.
        val dynamicInputHashSize = if (inputBits.nonEmpty) {
          val n = inputBits.size
          if (n <= params.minHashSize) n
          else Math.max(params.minHashSize, Math.min(params.maxInputHashSize, log2Ceil(n + 1)))
        } else 0
        val dynamicCoreHashSize = if (coreBitsAll.nonEmpty) {
          val n = coreBitsAll.size
          if (n <= params.minHashSize) n
          else Math.max(params.minHashSize, Math.min(params.maxCoreHashSize, log2Ceil(n + 1)))
        } else 0

        val (inputHash, inputHashStmts) = buildHash(inputBits, dynamicInputHashSize, s"${mName}_in")
        val (coreHash, coreHashStmts) = buildHash(coreBitsAll, dynamicCoreHashSize, s"${mName}_core")

        val addrWidth = dynamicInputHashSize + dynamicCoreHashSize
        val addrExpr = DoPrim(Cat, Seq(inputHash, coreHash), Seq(), UIntType(IntWidth(addrWidth)))

        val covSumPort = Port(NoInfo, "io_hierCovSum", Output, UIntType(IntWidth(params.covSumSize)))
        val covHashPort = Port(NoInfo, "io_hierCovHash", Output, UIntType(IntWidth(params.submodHashSize)))

        if (hasClockFound && addrWidth > 0) {
          if (params.bucketCount < 2) {
            throw new Exception("bucketCount must be >= 2")
          }
          val covMapSize = Math.pow(2, addrWidth).toInt
          val (covMap, covRef) = defMemory(s"${mName}_hierCov", s"Hierarchical coverage map for ${mName}", covMapSize, addrWidth)
          val covSum = DefRegister(NoInfo, s"${mName}_hierCovSum", UIntType(IntWidth(params.covSumSize)),
            WRef(clockName, ClockType, PortKind, SourceFlow),
            UIntLiteral(0, IntWidth(1)),
            WRef(s"${mName}_hierCovSum", UIntType(IntWidth(params.covSumSize)), RegKind, UnknownFlow))

          val bucketIdxWidth = log2Ceil(params.bucketCount)
          if ((1 << bucketIdxWidth) != params.bucketCount) {
            throw new Exception("bucketCount must be a power of 2")
          }
          val bucketRegs: Seq[DefRegister] = (0 until params.bucketCount).map { i =>
            DefRegister(NoInfo, s"${mName}_covBucket_${i}", UIntType(IntWidth(params.bucketWidth)),
              WRef(clockName, ClockType, PortKind, SourceFlow),
              UIntLiteral(0, IntWidth(1)),
              WRef(s"${mName}_covBucket_${i}", UIntType(IntWidth(params.bucketWidth)), RegKind, UnknownFlow))
          }

          val readSubField = WSubField(covRef, "read")
          val writeSubField = WSubField(covRef, "write")

          val rdAddr = Connect(NoInfo, WSubField(readSubField, "addr", UIntType(IntWidth(addrWidth)), SinkFlow), addrExpr)
          val rdEn = Connect(NoInfo, WSubField(readSubField, "en", UIntType(IntWidth(1)), SinkFlow), UIntLiteral(1, IntWidth(1)))
          val rdClk = Connect(NoInfo, WSubField(readSubField, "clk", ClockType, SinkFlow), WRef(clockName, ClockType, PortKind))

          val wrAddr = Connect(NoInfo, WSubField(writeSubField, "addr", UIntType(IntWidth(addrWidth)), SinkFlow), addrExpr)
          val wrMask = Connect(NoInfo, WSubField(writeSubField, "mask", UIntType(IntWidth(1)), SinkFlow), UIntLiteral(1, IntWidth(1)))
          val wrEn = Connect(NoInfo, WSubField(writeSubField, "en", UIntType(IntWidth(1)), SinkFlow), UIntLiteral(1, IntWidth(1)))
          val wrClk = Connect(NoInfo, WSubField(writeSubField, "clk", ClockType, SinkFlow), WRef(clockName, ClockType, PortKind))
          val wrData = Connect(NoInfo, WSubField(writeSubField, "data", UIntType(IntWidth(1))), UIntLiteral(1, IntWidth(1)))

          val readData = WSubField(readSubField, "data", UIntType(IntWidth(1)))
          val newHit = DoPrim(Not, Seq(readData), Seq(), UIntType(IntWidth(1)))

          val updateSum = Connect(NoInfo, WRef(covSum),
            Mux(readData,
              WRef(covSum),
              DoPrim(Add, Seq(WRef(covSum), UIntLiteral(1, IntWidth(1))), Seq(), UIntType(IntWidth(params.covSumSize)))
            ))

          val bucketIdx = xorFoldAddr(addrExpr, addrWidth, bucketIdxWidth)
          val bucketCons = bucketRegs.zipWithIndex.map { case (b, i) =>
            val idxLit = UIntLiteral(i, IntWidth(bucketIdxWidth))
            val isBucket = DoPrim(Eq, Seq(bucketIdx, idxLit), Seq(), UIntType(IntWidth(1)))
            val inc = DoPrim(Add, Seq(WRef(b), UIntLiteral(1, IntWidth(1))), Seq(), UIntType(IntWidth(params.bucketWidth)))
            val nextVal = Mux(newHit, Mux(isBucket, inc, WRef(b)), WRef(b))
            Connect(NoInfo, WRef(b), nextVal)
          }

          val covSumCon = Connect(NoInfo, WRef(covSumPort), WRef(covSum))

          val bucketBits = ListBuffer[(Expression, String)]()
          for ((b, i) <- bucketRegs.zipWithIndex) {
            val width = params.bucketWidth
            val stride = Math.max(1, width / Math.min(width, 8))
            val bitIdxs = (0 until width by stride)
            for (idx <- bitIdxs) {
              if (bucketBits.size >= params.maxBucketSigBits) {
                // stop collecting
              } else {
                bucketBits.append((bitExtract(WRef(b), idx, UIntType(IntWidth(width))), s"${mName}_covBucket_${i}[$idx]"))
              }
            }
          }

          val (covHash, covHashStmts) = buildHash(bucketBits.toSeq, params.submodHashSize, s"${mName}_covHash")
          val covHashCon = Connect(NoInfo, WRef(covHashPort), covHash)

          val ports = m.ports ++ Seq(covSumPort, covHashPort)
          val newStmts = stmts ++ inputHashStmts ++ coreHashStmts ++ Seq(covMap, covSum) ++
            bucketRegs ++ Seq(rdAddr, rdEn, rdClk, wrAddr, wrMask, wrEn, wrClk, wrData, updateSum) ++
            bucketCons ++ Seq(covSumCon) ++ covHashStmts :+ covHashCon

          Module(m.info, mName, ports, Block(newStmts))
        } else {
          val covSumWire = DefWire(NoInfo, s"${mName}_hierCovSum_wire", UIntType(IntWidth(params.covSumSize)))
          val covHashWire = DefWire(NoInfo, s"${mName}_hierCovHash_wire", UIntType(IntWidth(params.submodHashSize)))
          val covSumZero = Connect(NoInfo, WRef(covSumWire), UIntLiteral(0, IntWidth(params.covSumSize)))
          val covHashZero = Connect(NoInfo, WRef(covHashWire), UIntLiteral(0, IntWidth(params.submodHashSize)))
          val covSumCon = Connect(NoInfo, WRef(covSumPort), WRef(covSumWire))
          val covHashCon = Connect(NoInfo, WRef(covHashPort), WRef(covHashWire))

          val ports = m.ports ++ Seq(covSumPort, covHashPort)
          val newStmts = stmts ++ Seq(covSumWire, covHashWire, covSumZero, covHashZero, covSumCon, covHashCon)
          Module(m.info, mName, ports, Block(newStmts))
        }

      case ext: ExtModule => ext
      case other => other
    }
  }
}

class InstrHierAssertV7(mod: DefModule, mInfo: HierModuleInfoV7) {
  private val mName = mod.name
  private val insts = mInfo.insts

  def instrument(): DefModule = {
    mod match {
      case m: Module =>
        val block = m.body
        val stmts = block.asInstanceOf[Block].stmts
        val (clockName, resetName, hasClockReset) = hasClockAndReset(m)

        val assertPort = Port(NoInfo, "metaAssert", Output, UIntType(IntWidth(1)))

        val stops = ListBuffer[Stop]()
        findStop(stops)(block)

        val stopEns = stops.zipWithIndex.map(tup => DefNode(NoInfo, s"stopEn${tup._2}", tup._1.en)).toSeq
        val instAsserts = insts.map(inst =>
          (inst, DefWire(NoInfo, s"${inst.name}_metaAssert_wire", UIntType(IntWidth(1))))).toSeq
        val instAssertCons = instAsserts.map(tup =>
          Connect(NoInfo, WRef(tup._2), WSubField(WRef(tup._1), "metaAssert"))
        )

        val (topOr, orStmts) =
          makeOr(stopEns.map(en => WRef(en)) ++ instAsserts.map(tup => WRef(tup._2)), 0)

        val conStmts = if (hasClockReset && topOr != None) {
          val assertReg = DefRegister(NoInfo, s"${mName}_metaAssert", UIntType(IntWidth(1)),
            WRef(clockName, ClockType, PortKind, SourceFlow),
            UIntLiteral(0, IntWidth(1)),
            WRef(s"${mName}_metaAssert", UIntType(IntWidth(1)), RegKind, UnknownFlow))
          val or = DoPrim(Or, Seq(WRef(assertReg), topOr.get), Seq(), UIntType(IntWidth(1)))
          val assertRegCon = Connect(NoInfo, WRef(assertReg), or)
          val portCon = Connect(NoInfo, WRef(assertPort), WRef(assertReg))
          Seq[Statement](assertReg, assertRegCon, portCon)
        } else if (topOr != None) {
          val portCon = Connect(NoInfo, WRef(assertPort), topOr.get)
          Seq[Statement](portCon)
        } else {
          val portCon = Connect(NoInfo, WRef(assertPort), UIntLiteral(0))
          Seq[Statement](portCon)
        }

        val ports = (m.ports :+ assertPort)
        val newStmts = stmts ++ stopEns ++ instAsserts.map(tup => tup._2) ++ instAssertCons ++ orStmts ++ conStmts
        val newBlock = Block(newStmts)
        Module(m.info, mName, ports, newBlock)
      case ext: ExtModule => ext
      case other => other
    }
  }

  def makeOr(stopEns: Seq[WRef], id: Int): (Option[WRef], Seq[Statement]) = {
    stopEns.length match {
      case 0 => (None, Seq[Statement]())
      case 1 => (Some(stopEns.head), Seq[Statement]())
      case 2 =>
        val orWire = DefWire(NoInfo, mName + s"_or${id}", UIntType(IntWidth(1)))
        val orOp = DoPrim(Or, stopEns, Seq(), UIntType(IntWidth(1)))
        val orCon = Connect(NoInfo, WRef(orWire), orOp)
        (Some(WRef(orWire)), Seq[Statement](orWire, orCon))
      case _ =>
        val (or1, stmts1) = makeOr(stopEns.splitAt(stopEns.length / 2)._1, 2 * id + 1)
        val (or2, stmts2) = makeOr(stopEns.splitAt(stopEns.length / 2)._2, 2 * id + 2)
        val orWire = DefWire(NoInfo, mName + s"_or${id}", UIntType(IntWidth(1)))
        val orOp = DoPrim(Or, Seq(or1.get, or2.get), Seq(), UIntType(IntWidth(1)))
        val orCon = Connect(NoInfo, WRef(orWire), orOp)
        (Some(WRef(orWire)), stmts1 ++ stmts2 :+ orWire :+ orCon)
    }
  }

  def findStop(stops: ListBuffer[Stop])(stmt: Statement): Unit = stmt match {
    case stop: Stop => stops.append(stop)
    case s: Statement => s foreachStmt findStop(stops)
  }

  def hasClockAndReset(mod: Module): (String, String, Boolean) = {
    val ports = mod.ports
    val (clockName, resetName) = ports.foldLeft[(String, String)](("None", "None"))(
      (tuple, p) => {
        if (p.name == "clock") (p.name, tuple._2)
        else if (p.name contains "reset") (tuple._1, p.name)
        else tuple
      })
    val hasClockAndReset = (clockName != "None") && (resetName != "None")
    (clockName, resetName, hasClockAndReset)
  }
}

class InstrHierResetV7(mod: DefModule, mInfo: HierModuleInfoV7, moduleInfos: scala.collection.Map[String, HierModuleInfoV7]) {
  private val mName = mod.name
  private val insts = mInfo.insts
  private val regs = mInfo.regs   // ALL regs — metaReset must reset every register

  def instrument(): DefModule = {
    mod match {
      case m: Module =>
        val stmts = m.body.asInstanceOf[Block].stmts
        val metaResetPort = Port(NoInfo, "metaReset", Input, UIntType(IntWidth(1)))
        val newStmts = stmts.map(addMetaReset(metaResetPort))

        val resetCons = ListBuffer[Statement]()
        val instResetPorts = ListBuffer[Port]()
        val childInstHaltCons = ListBuffer[Statement]()
        for (inst <- insts) {
          val instResetPort = Port(NoInfo, inst.name + "_halt", Input, UIntType(IntWidth(1)))
          val instReset = DoPrim(Or, Seq(WRef(metaResetPort), WRef(instResetPort)), Seq(), UIntType(IntWidth(1)))
          val instResetCon = Connect(NoInfo, WSubField(WRef(inst), "metaReset"), instReset)

          resetCons.append(instResetCon)
          instResetPorts.append(instResetPort)

          moduleInfos.get(inst.module).foreach { childInfo =>
            for (childInst <- childInfo.insts) {
              val haltRef = WSubField(WRef(inst), childInst.name + "_halt")
              childInstHaltCons.append(Connect(NoInfo, haltRef, UIntLiteral(0, IntWidth(1))))
            }
          }
        }

        val ports = (m.ports :+ metaResetPort) ++ instResetPorts
        val newBlock = Block(newStmts ++ resetCons ++ childInstHaltCons)

        Module(m.info, mName, ports, newBlock)
      case ext: ExtModule => ext
      case other => other
    }
  }

  def addMetaReset(metaReset: Port)(s: Statement): Statement = {
    s match {
      case Connect(info, loc, expr) if regs.exists(r => r.name == loc.serialize) =>
        val reg = regs.find(r => r.name == loc.serialize).getOrElse(
          throw new Exception(s"${loc.serialize} is not in registers")
        )
        reg.tpe match {
          case utpe: UIntType =>
            utpe.width match {
              case w: IntWidth => Connect(info, loc, Mux(WRef(metaReset), UIntLiteral(0, IntWidth(w.width)), expr))
              case _ => s
            }
          case stpe: SIntType =>
            stpe.width match {
              case w: IntWidth => Connect(info, loc, Mux(WRef(metaReset), SIntLiteral(0, IntWidth(w.width)), expr))
              case _ => s
            }
          case _ => s
        }
      case other =>
        other.mapStmt(addMetaReset(metaReset))
    }
  }
}

class hierCoverage_v7 extends Transform {
  def inputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm
  def outputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm

  private val moduleInfos = mutable.Map[String, HierModuleInfoV7]()
  private val params = HierCovParamsV7()

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit

    for (m <- circuit.modules) {
      val gLedger = new graphLedger(m)
      gLedger.parseModule
      moduleInfos(m.name) = HierModuleInfoV7(m, gLedger)
    }

    val extModules = circuit.modules.filter(_.isInstanceOf[ExtModule]).map(_.name).toSet
    val extModulePorts = mutable.Map[String, Seq[Port]]()
    for (m <- circuit.modules) m match {
      case ext: ExtModule => extModulePorts(ext.name) = ext.ports
      case _ =>
    }

    val instrCircuit = circuit map { m: DefModule =>
      val instrCov = new InstrHierCovV7(m, moduleInfos(m.name), extModules, extModulePorts, params)
      instrCov.instrument()
    }

    val assertCircuit = instrCircuit map { m: DefModule =>
      val instrAssert = new InstrHierAssertV7(m, moduleInfos(m.name))
      instrAssert.instrument()
    }

    val metaResetCircuit = assertCircuit map { m: DefModule =>
      val instrReset = new InstrHierResetV7(m, moduleInfos(m.name), moduleInfos)
      instrReset.instrument()
    }

    writeCoverageSummary(circuit, extModules, extModulePorts, metaResetCircuit.main)

    state.copy(metaResetCircuit)
  }

  private def writeCoverageSummary(circuit: Circuit, extModules: Set[String], extModulePorts: scala.collection.Map[String, Seq[Port]], topName: String): Unit = {
    val moduleMap = circuit.modules.map(m => m.name -> m).toMap
    val moduleNums: Map[String, Int] = moduleInfos.map(tuple => {
      (tuple._1, findModules(topName, tuple._1))
    }).toMap

    def dynamicHash(n: Int, maxH: Int): Int = {
      if (n <= 0) 0
      else if (n <= params.minHashSize) n
      else Math.max(params.minHashSize, Math.min(maxH, HierCovUtilV7.log2Ceil(n + 1)))
    }

    def covMapSizeOf(moduleName: String): Long = {
      moduleMap.get(moduleName) match {
        case Some(m: Module) =>
          val (_, hasClk) = HierCovUtilV7.hasClock(m)
          if (!hasClk) {
            0L
          } else {
            val mInfo = moduleInfos(moduleName)
            val inputBits = HierCovUtilV7.selectDataInputBits(m.ports, mInfo.ctrlPortNames, params)
            val regBits = HierCovUtilV7.selectControlRegBits(mInfo.ctrlRegs, mInfo.dirInRegs, params)
            val submodInsts = mInfo.insts.count(inst => !extModules.contains(inst.module))
            val extmodInsts = mInfo.insts.filter(inst => extModules.contains(inst.module))
            val extmodBitCount = extmodInsts.foldLeft(0) { (acc, inst) =>
              acc + extModulePorts.getOrElse(inst.module, Seq.empty)
                .count(p => p.direction == Input && (p.tpe.isInstanceOf[UIntType] || p.tpe.isInstanceOf[SIntType]))
                .min(params.maxExtModPorts) * params.maxExtModBitsPerPort
            }
            val inputHashEff = dynamicHash(inputBits.size, params.maxInputHashSize)
            val coreBitCount = regBits.size + submodInsts * params.submodHashSize + extmodBitCount
            val coreHashEff = dynamicHash(coreBitCount, params.maxCoreHashSize)
            val addrWidth = inputHashEff + coreHashEff
            if (addrWidth > 0) (1L << addrWidth) else 0L
          }
        case _ => 0L
      }
    }

    val perModule = moduleInfos.keys.toSeq.sorted.map { mName =>
      val covSize = covMapSizeOf(mName)
      val instCnt = moduleNums.getOrElse(mName, 0)
      val mInfo = moduleInfos(mName)
      val ctrlRegCount = mInfo.ctrlRegs.size
      val totalRegCount = mInfo.regs.size
      // Compute dynamic hash sizes for summary reporting
      val (inputHashH, coreHashH, extBitCount) = moduleMap.get(mName) match {
        case Some(m: Module) =>
          val inputBits = HierCovUtilV7.selectDataInputBits(m.ports, mInfo.ctrlPortNames, params)
          val regBits = HierCovUtilV7.selectControlRegBits(mInfo.ctrlRegs, mInfo.dirInRegs, params)
          val submodInsts = mInfo.insts.count(inst => !extModules.contains(inst.module))
          val extmodInsts = mInfo.insts.filter(inst => extModules.contains(inst.module))
          val ebc = extmodInsts.foldLeft(0) { (acc, inst) =>
            acc + extModulePorts.getOrElse(inst.module, Seq.empty)
              .count(p => p.direction == Input && (p.tpe.isInstanceOf[UIntType] || p.tpe.isInstanceOf[SIntType]))
              .min(params.maxExtModPorts) * params.maxExtModBitsPerPort
          }
          val ih = dynamicHash(inputBits.size, params.maxInputHashSize)
          val coreBitCount = regBits.size + submodInsts * params.submodHashSize + ebc
          val ch = dynamicHash(coreBitCount, params.maxCoreHashSize)
          (ih, ch, ebc)
        case _ => (0, 0, 0)
      }
      s"  ${mName}: covMapSize=${covSize}, inputHash=${inputHashH}, coreHash=${coreHashH}, ctrlRegs=${ctrlRegCount}, totalRegs=${totalRegCount}, extProxyBits=${extBitCount}, instances=${instCnt}\n"
    }
    val totalCov = moduleInfos.keys.toSeq.foldLeft(0L) { (acc, mName) =>
      acc + covMapSizeOf(mName) * moduleNums.getOrElse(mName, 0).toLong
    }

    val text =
      s"Top module: ${topName}\n" +
      s"Total coverage points (hier_cov_v7 data-input): ${totalCov}\n" +
      "Per-module coverage points:\n" +
      perModule.mkString("")

    val named = new PrintWriter(new File(s"${topName}_hier_cov_summary.txt"))
    named.write(text)
    named.close()

    val compat = new PrintWriter(new File("summary.txt"))
    compat.write(text)
    compat.close()
  }

  private def findModules(topName: String, moduleName: String): Int = {
    if (topName == moduleName) 1
    else {
      moduleInfos.get(topName).map(_.insts.foldLeft(0)((num, inst) => num + findModules(inst.module, moduleName))).getOrElse(0)
    }
  }
}
