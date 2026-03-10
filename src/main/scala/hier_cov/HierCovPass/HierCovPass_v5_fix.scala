package hier_cov

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._
import firrtl2.PrimOps._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import coverage.graphLedger

// v5_fix: same as v5 (all input ports, larger hash sizes) but fixes the SInt
// metaReset bug — uses SIntLiteral(0) instead of UIntLiteral(0) when resetting
// SInt-typed registers, avoiding the FIRRTL type-mismatch error.
case class HierCovParamsV5Fix(
  inputHashSize: Int = 8,
  coreHashSize: Int = 10,
  submodHashSize: Int = 6,
  covSumSize: Int = 32,
  maxBitsPerPort: Int = 8,
  maxRegBits: Int = 64,
  bucketCount: Int = 16,
  bucketWidth: Int = 8,
  maxBucketSigBits: Int = 128
)

object HierCovUtilV5Fix {
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
    while (v > 0) { v = v >> 1; r = r + 1 }
    r
  }

  def selectInputBits(ports: Seq[Port], params: HierCovParamsV5Fix): Seq[(Expression, String)] = {
    val dataInputs = ports.filter(p => p.direction == Input).filterNot { p =>
      val lname = p.name.toLowerCase
      p.tpe == ClockType || lname.contains("clock") || lname.contains("reset")
    }.filter(p => p.tpe.isInstanceOf[UIntType] || p.tpe.isInstanceOf[SIntType])

    dataInputs.flatMap { p =>
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

  def selectRegBits(regs: Seq[DefRegister], params: HierCovParamsV5Fix): Seq[(Expression, String)] = {
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

class HierModuleInfoV5Fix(val mName: String,
                          val insts: scala.collection.Set[WDefInstance],
                          val regs: scala.collection.Set[DefRegister])

object HierModuleInfoV5Fix {
  def apply(mod: DefModule, gLedger: graphLedger): HierModuleInfoV5Fix = {
    val insts = gLedger.getInstances
    val regs = gLedger.findRegs
    new HierModuleInfoV5Fix(mod.name, insts, regs)
  }
}

class InstrHierCovV5Fix(mod: DefModule,
                        mInfo: HierModuleInfoV5Fix,
                        extModules: Set[String],
                        params: HierCovParamsV5Fix) {
  import HierCovUtilV5Fix._

  private val mName = mod.name

  def instrument(): DefModule = {
    mod match {
      case m: Module =>
        val stmts = m.body.asInstanceOf[Block].stmts
        val (clockName, hasClockFound) = hasClock(m)

        val inputBits = selectInputBits(m.ports, params)
        val regBits = selectRegBits(mInfo.regs.toSeq, params)

        val submodBits = mInfo.insts.filter(inst => !extModules.contains(inst.module)).flatMap { inst =>
          val ref = WSubField(WRef(inst), "io_hierCovHash")
          (0 until params.submodHashSize).map { idx =>
            val bit = bitExtract(ref, idx, UIntType(IntWidth(params.submodHashSize)))
            (bit, s"${inst.name}.io_hierCovHash[$idx]")
          }
        }.toSeq

        val (inputHash, inputHashStmts) = buildHash(inputBits, params.inputHashSize, s"${mName}_in")
        val (coreHash, coreHashStmts) = buildHash(regBits ++ submodBits, params.coreHashSize, s"${mName}_core")

        val addrWidth = params.inputHashSize + params.coreHashSize
        val addrExpr = DoPrim(Cat, Seq(inputHash, coreHash), Seq(), UIntType(IntWidth(addrWidth)))

        val covSumPort = Port(NoInfo, "io_hierCovSum", Output, UIntType(IntWidth(params.covSumSize)))
        val covHashPort = Port(NoInfo, "io_hierCovHash", Output, UIntType(IntWidth(params.submodHashSize)))

        if (hasClockFound && addrWidth > 0) {
          if (params.bucketCount < 2) throw new Exception("bucketCount must be >= 2")
          val covMapSize = Math.pow(2, addrWidth).toInt
          val (covMap, covRef) = defMemory(s"${mName}_hierCov", s"Hierarchical coverage map for ${mName}", covMapSize, addrWidth)
          val covSum = DefRegister(NoInfo, s"${mName}_hierCovSum", UIntType(IntWidth(params.covSumSize)),
            WRef(clockName, ClockType, PortKind, SourceFlow),
            UIntLiteral(0, IntWidth(1)),
            WRef(s"${mName}_hierCovSum", UIntType(IntWidth(params.covSumSize)), RegKind, UnknownFlow))

          val bucketIdxWidth = log2Ceil(params.bucketCount)
          if ((1 << bucketIdxWidth) != params.bucketCount) throw new Exception("bucketCount must be a power of 2")
          val bucketRegs: Seq[DefRegister] = (0 until params.bucketCount).map { i =>
            DefRegister(NoInfo, s"${mName}_covBucket_${i}", UIntType(IntWidth(params.bucketWidth)),
              WRef(clockName, ClockType, PortKind, SourceFlow),
              UIntLiteral(0, IntWidth(1)),
              WRef(s"${mName}_covBucket_${i}", UIntType(IntWidth(params.bucketWidth)), RegKind, UnknownFlow))
          }

          val readSubField  = WSubField(covRef, "read")
          val writeSubField = WSubField(covRef, "write")

          val rdAddr = Connect(NoInfo, WSubField(readSubField, "addr", UIntType(IntWidth(addrWidth)), SinkFlow), addrExpr)
          val rdEn   = Connect(NoInfo, WSubField(readSubField, "en",   UIntType(IntWidth(1)), SinkFlow), UIntLiteral(1, IntWidth(1)))
          val rdClk  = Connect(NoInfo, WSubField(readSubField, "clk",  ClockType, SinkFlow), WRef(clockName, ClockType, PortKind))

          val wrAddr = Connect(NoInfo, WSubField(writeSubField, "addr", UIntType(IntWidth(addrWidth)), SinkFlow), addrExpr)
          val wrMask = Connect(NoInfo, WSubField(writeSubField, "mask", UIntType(IntWidth(1)), SinkFlow), UIntLiteral(1, IntWidth(1)))
          val wrEn   = Connect(NoInfo, WSubField(writeSubField, "en",   UIntType(IntWidth(1)), SinkFlow), UIntLiteral(1, IntWidth(1)))
          val wrClk  = Connect(NoInfo, WSubField(writeSubField, "clk",  ClockType, SinkFlow), WRef(clockName, ClockType, PortKind))
          val wrData = Connect(NoInfo, WSubField(writeSubField, "data", UIntType(IntWidth(1))), UIntLiteral(1, IntWidth(1)))

          val readData = WSubField(readSubField, "data", UIntType(IntWidth(1)))
          val newHit   = DoPrim(Not, Seq(readData), Seq(), UIntType(IntWidth(1)))

          val updateSum = Connect(NoInfo, WRef(covSum),
            Mux(readData,
              WRef(covSum),
              DoPrim(Add, Seq(WRef(covSum), UIntLiteral(1, IntWidth(1))), Seq(), UIntType(IntWidth(params.covSumSize)))
            ))

          val bucketIdx = xorFoldAddr(addrExpr, addrWidth, bucketIdxWidth)
          val bucketCons = bucketRegs.zipWithIndex.map { case (b, i) =>
            val idxLit   = UIntLiteral(i, IntWidth(bucketIdxWidth))
            val isBucket = DoPrim(Eq, Seq(bucketIdx, idxLit), Seq(), UIntType(IntWidth(1)))
            val inc      = DoPrim(Add, Seq(WRef(b), UIntLiteral(1, IntWidth(1))), Seq(), UIntType(IntWidth(params.bucketWidth)))
            val nextVal  = Mux(newHit, Mux(isBucket, inc, WRef(b)), WRef(b))
            Connect(NoInfo, WRef(b), nextVal)
          }

          val covSumCon = Connect(NoInfo, WRef(covSumPort), WRef(covSum))

          val bucketBits = ListBuffer[(Expression, String)]()
          for ((b, i) <- bucketRegs.zipWithIndex) {
            val width  = params.bucketWidth
            val stride = Math.max(1, width / Math.min(width, 8))
            for (idx <- 0 until width by stride) {
              if (bucketBits.size < params.maxBucketSigBits)
                bucketBits.append((bitExtract(WRef(b), idx, UIntType(IntWidth(width))), s"${mName}_covBucket_${i}[$idx]"))
            }
          }

          val (covHash, covHashStmts) = buildHash(bucketBits.toSeq, params.submodHashSize, s"${mName}_covHash")
          val covHashCon = Connect(NoInfo, WRef(covHashPort), covHash)

          val ports    = m.ports ++ Seq(covSumPort, covHashPort)
          val newStmts = stmts ++ inputHashStmts ++ coreHashStmts ++ Seq(covMap, covSum) ++
            bucketRegs ++ Seq(rdAddr, rdEn, rdClk, wrAddr, wrMask, wrEn, wrClk, wrData, updateSum) ++
            bucketCons ++ Seq(covSumCon) ++ covHashStmts :+ covHashCon

          Module(m.info, mName, ports, Block(newStmts))
        } else {
          val covSumWire  = DefWire(NoInfo, s"${mName}_hierCovSum_wire", UIntType(IntWidth(params.covSumSize)))
          val covHashWire = DefWire(NoInfo, s"${mName}_hierCovHash_wire", UIntType(IntWidth(params.submodHashSize)))
          val ports    = m.ports ++ Seq(covSumPort, covHashPort)
          val newStmts = stmts ++ Seq(
            covSumWire, covHashWire,
            Connect(NoInfo, WRef(covSumWire),  UIntLiteral(0, IntWidth(params.covSumSize))),
            Connect(NoInfo, WRef(covHashWire), UIntLiteral(0, IntWidth(params.submodHashSize))),
            Connect(NoInfo, WRef(covSumPort),  WRef(covSumWire)),
            Connect(NoInfo, WRef(covHashPort), WRef(covHashWire))
          )
          Module(m.info, mName, ports, Block(newStmts))
        }

      case ext: ExtModule => ext
      case other => other
    }
  }
}

class InstrHierAssertV5Fix(mod: DefModule, mInfo: HierModuleInfoV5Fix) {
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

        val stopEns     = stops.zipWithIndex.map(tup => DefNode(NoInfo, s"stopEn${tup._2}", tup._1.en)).toSeq
        val instAsserts = insts.map(inst =>
          (inst, DefWire(NoInfo, s"${inst.name}_metaAssert_wire", UIntType(IntWidth(1))))).toSeq
        val instAssertCons = instAsserts.map(tup =>
          Connect(NoInfo, WRef(tup._2), WSubField(WRef(tup._1), "metaAssert")))

        val (topOr, orStmts) =
          makeOr(stopEns.map(en => WRef(en)) ++ instAsserts.map(tup => WRef(tup._2)), 0)

        val conStmts = if (hasClockReset && topOr != None) {
          val assertReg = DefRegister(NoInfo, s"${mName}_metaAssert", UIntType(IntWidth(1)),
            WRef(clockName, ClockType, PortKind, SourceFlow),
            UIntLiteral(0, IntWidth(1)),
            WRef(s"${mName}_metaAssert", UIntType(IntWidth(1)), RegKind, UnknownFlow))
          val or           = DoPrim(Or, Seq(WRef(assertReg), topOr.get), Seq(), UIntType(IntWidth(1)))
          val assertRegCon = Connect(NoInfo, WRef(assertReg), or)
          val portCon      = Connect(NoInfo, WRef(assertPort), WRef(assertReg))
          Seq[Statement](assertReg, assertRegCon, portCon)
        } else if (topOr != None) {
          Seq[Statement](Connect(NoInfo, WRef(assertPort), topOr.get))
        } else {
          Seq[Statement](Connect(NoInfo, WRef(assertPort), UIntLiteral(0)))
        }

        val ports    = m.ports :+ assertPort
        val newStmts = stmts ++ stopEns ++ instAsserts.map(_._2) ++ instAssertCons ++ orStmts ++ conStmts
        Module(m.info, mName, ports, Block(newStmts))

      case ext: ExtModule => ext
      case other => other
    }
  }

  def makeOr(stopEns: Seq[WRef], id: Int): (Option[WRef], Seq[Statement]) = stopEns.length match {
    case 0 => (None, Seq())
    case 1 => (Some(stopEns.head), Seq())
    case 2 =>
      val orWire = DefWire(NoInfo, mName + s"_or${id}", UIntType(IntWidth(1)))
      val orCon  = Connect(NoInfo, WRef(orWire), DoPrim(Or, stopEns, Seq(), UIntType(IntWidth(1))))
      (Some(WRef(orWire)), Seq(orWire, orCon))
    case _ =>
      val (or1, s1) = makeOr(stopEns.splitAt(stopEns.length / 2)._1, 2 * id + 1)
      val (or2, s2) = makeOr(stopEns.splitAt(stopEns.length / 2)._2, 2 * id + 2)
      val orWire = DefWire(NoInfo, mName + s"_or${id}", UIntType(IntWidth(1)))
      val orCon  = Connect(NoInfo, WRef(orWire), DoPrim(Or, Seq(or1.get, or2.get), Seq(), UIntType(IntWidth(1))))
      (Some(WRef(orWire)), s1 ++ s2 :+ orWire :+ orCon)
  }

  def findStop(stops: ListBuffer[Stop])(stmt: Statement): Unit = stmt match {
    case stop: Stop => stops.append(stop)
    case s: Statement => s foreachStmt findStop(stops)
  }

  def hasClockAndReset(mod: Module): (String, String, Boolean) = {
    val ports = mod.ports
    val (clockName, resetName) = ports.foldLeft(("None", "None")) { (acc, p) =>
      if (p.name == "clock") (p.name, acc._2)
      else if (p.name.contains("reset")) (acc._1, p.name)
      else acc
    }
    (clockName, resetName, clockName != "None" && resetName != "None")
  }
}

class InstrHierResetV5Fix(mod: DefModule,
                          mInfo: HierModuleInfoV5Fix,
                          moduleInfos: scala.collection.Map[String, HierModuleInfoV5Fix]) {
  private val mName = mod.name
  private val insts = mInfo.insts
  private val regs  = mInfo.regs

  def instrument(): DefModule = {
    mod match {
      case m: Module =>
        val stmts         = m.body.asInstanceOf[Block].stmts
        val metaResetPort = Port(NoInfo, "metaReset", Input, UIntType(IntWidth(1)))
        val newStmts      = stmts.map(addMetaReset(metaResetPort))

        val resetCons         = ListBuffer[Statement]()
        val instResetPorts    = ListBuffer[Port]()
        val childInstHaltCons = ListBuffer[Statement]()

        for (inst <- insts) {
          val instResetPort = Port(NoInfo, inst.name + "_halt", Input, UIntType(IntWidth(1)))
          val instReset     = DoPrim(Or, Seq(WRef(metaResetPort), WRef(instResetPort)), Seq(), UIntType(IntWidth(1)))
          val instResetCon  = Connect(NoInfo, WSubField(WRef(inst), "metaReset"), instReset)
          resetCons.append(instResetCon)
          instResetPorts.append(instResetPort)

          moduleInfos.get(inst.module).foreach { childInfo =>
            for (childInst <- childInfo.insts) {
              val haltRef = WSubField(WRef(inst), childInst.name + "_halt")
              childInstHaltCons.append(Connect(NoInfo, haltRef, UIntLiteral(0, IntWidth(1))))
            }
          }
        }

        val ports    = (m.ports :+ metaResetPort) ++ instResetPorts
        val newBlock = Block(newStmts ++ resetCons ++ childInstHaltCons)
        Module(m.info, mName, ports, newBlock)

      case ext: ExtModule => ext
      case other => other
    }
  }

  def addMetaReset(metaReset: Port)(s: Statement): Statement = s match {
    case Connect(info, loc, expr) if regs.exists(r => r.name == loc.serialize) =>
      val reg = regs.find(r => r.name == loc.serialize).get
      reg.tpe match {
        case UIntType(IntWidth(w)) => Connect(info, loc, Mux(WRef(metaReset), UIntLiteral(0, IntWidth(w)), expr))
        case SIntType(IntWidth(w)) => Connect(info, loc, Mux(WRef(metaReset), SIntLiteral(0, IntWidth(w)), expr))
        case _ => s
      }
    case other => other.mapStmt(addMetaReset(metaReset))
  }
}

class hierCoverage_v5_fix extends Transform {
  def inputForm: firrtl2.stage.Forms.LowForm.type  = firrtl2.stage.Forms.LowForm
  def outputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm

  private val moduleInfos = mutable.Map[String, HierModuleInfoV5Fix]()
  private val params      = HierCovParamsV5Fix()

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit

    for (m <- circuit.modules) {
      val gLedger = new graphLedger(m)
      gLedger.parseModule
      moduleInfos(m.name) = HierModuleInfoV5Fix(m, gLedger)
    }

    val extModules = circuit.modules.filter(_.isInstanceOf[ExtModule]).map(_.name).toSet

    val instrCircuit = circuit map { m: DefModule =>
      new InstrHierCovV5Fix(m, moduleInfos(m.name), extModules, params).instrument()
    }

    val assertCircuit = instrCircuit map { m: DefModule =>
      new InstrHierAssertV5Fix(m, moduleInfos(m.name)).instrument()
    }

    val metaResetCircuit = assertCircuit map { m: DefModule =>
      new InstrHierResetV5Fix(m, moduleInfos(m.name), moduleInfos).instrument()
    }

    state.copy(metaResetCircuit)
  }
}
