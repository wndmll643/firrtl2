// v10a: data-driven selection variant of v9a. Replaces v9a's static
// stride-decimated sampling with external, per-module bit-index selections
// delivered via HierCovSelectionAnnotation (produced by
// tools/hiercov_select/analyze.py from a pre-run VCD).
//
// Everything downstream of selection — buildDirectOrFold, bucket histogram,
// io_hierCovSum / io_hierCovHash ports, metaAssert, metaReset — is unchanged
// from v9a. The ONLY behavioral difference is which reg/port bits participate
// in the hash.
//
// Requires: exactly one HierCovSelectionAnnotation in state.annotations.
// No fallback — missing annotation is a hard error.
package hier_cov

import java.io.{File, PrintWriter}

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._
import firrtl2.PrimOps._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import coverage.graphLedger

case class HierCovParamsV10a(
  maxInputHashSize: Int = 6,
  maxCoreHashSize:  Int = 14,
  maxAddrWidth:     Int = 20,
  submodHashSize:   Int = 16,
  covSumSize:       Int = 32,
  maxInputPorts:    Int = 8,    // post-selection cap; usually unhit
  maxBitsPerPort:   Int = 8,    // post-selection cap; usually unhit
  maxRegBits:       Int = 64,
  maxCtrlRegWidth:  Int = 20,
  bucketCount:      Int = 16,
  bucketWidth:      Int = 8,
  maxBucketSigBits: Int = 128
)

object HierCovUtilV10a {
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
    case _                     => None
  }

  def bitExtract(expr: Expression, idx: Int, tpe: Type): Expression = {
    val width = typeWidthOpt(tpe).getOrElse(
      throw new Exception(s"Unsupported type width: ${tpe.serialize}")
    )
    val asUInt = tpe match {
      case UIntType(_) => expr
      case SIntType(_) => DoPrim(AsUInt, Seq(expr), Seq(), UIntType(IntWidth(width)))
      case _           => throw new Exception(s"Unsupported type for bitExtract: ${tpe.serialize}")
    }
    DoPrim(Bits, Seq(asUInt), Seq(idx, idx), UIntType(IntWidth(1)))
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

  /** Build input bits from the annotation's kept-port list for this module.
    * Only Input-direction UInt/SInt ports named in `sel.ports` are sampled.
    * Bit indices come directly from the annotation.
    */
  def selectInputBitsFromAnno(
    ports: Seq[Port],
    sel:   ModuleSelection
  ): Seq[(Expression, String)] = {
    val byName = ports
      .filter(p => p.direction == Input)
      .filter(p => p.tpe.isInstanceOf[UIntType] || p.tpe.isInstanceOf[SIntType])
      .map(p => p.name -> p)
      .toMap

    val out = ListBuffer[(Expression, String)]()
    for ((pname, bitIdxs) <- sel.ports) {
      byName.get(pname).foreach { p =>
        typeWidthOpt(p.tpe).foreach { width =>
          for (idx <- bitIdxs if idx >= 0 && idx < width) {
            out.append((bitExtract(WRef(p), idx, p.tpe), s"${p.name}[$idx]"))
          }
        }
      }
    }
    out.toSeq
  }

  /** Build register bits from the annotation's kept-reg list for this module. */
  def selectRegBitsFromAnno(
    regs: scala.collection.Set[DefRegister],
    sel:  ModuleSelection
  ): Seq[(Expression, String)] = {
    val byName = regs.map(r => r.name -> r).toMap
    val out = ListBuffer[(Expression, String)]()
    for ((rname, bitIdxs) <- sel.regs) {
      byName.get(rname).foreach { r =>
        typeWidthOpt(r.tpe).foreach { width =>
          for (idx <- bitIdxs if idx >= 0 && idx < width) {
            out.append((bitExtract(WRef(r), idx, r.tpe), s"${r.name}[$idx]"))
          }
        }
      }
    }
    out.toSeq
  }

  /** Identical to v9a's buildDirectOrFold. Duplicated here to keep v10a
    * self-contained and decouple lifecycle from v9a.
    */
  def buildDirectOrFold(bits: Seq[(Expression, String)], outWidth: Int, baseName: String): (Expression, Seq[Statement]) = {
    if (outWidth <= 0 || bits.isEmpty) {
      return (UIntLiteral(0, IntWidth(Math.max(1, outWidth))), Seq[Statement]())
    }

    val sortedBits = bits.sortBy { case (_, name) => stableHash(name) }
    val numBits = sortedBits.size

    if (numBits <= outWidth) {
      val allBitExprs = sortedBits.map(_._1)
      val padded = allBitExprs ++ Seq.fill(outWidth - numBits)(UIntLiteral(0, IntWidth(1)))
      val concatWire = DefWire(NoInfo, s"${baseName}_hash", UIntType(IntWidth(outWidth)))
      val catExpr = catBits(padded.reverse)
      val concatCon = Connect(NoInfo, WRef(concatWire), catExpr)
      (WRef(concatWire), Seq(concatWire, concatCon))
    } else {
      val allBitExprs = sortedBits.map(_._1)
      val wideWire = DefWire(NoInfo, s"${baseName}_wide", UIntType(IntWidth(numBits)))
      val wideCat = catBits(allBitExprs.reverse)
      val wideCon = Connect(NoInfo, WRef(wideWire), wideCat)

      val foldedExpr = xorFoldAddr(WRef(wideWire), numBits, outWidth)
      val hashWire = DefWire(NoInfo, s"${baseName}_hash", UIntType(IntWidth(outWidth)))
      val hashCon = Connect(NoInfo, WRef(hashWire), foldedExpr)

      (WRef(hashWire), Seq(wideWire, wideCon, hashWire, hashCon))
    }
  }

  def defMemory(name: String, info: String, size: Int, addrWidth: Int): (DefMemory, WRef) = {
    val mem = DefMemory(FileInfo(StringLit(info)), name,
      UIntType(IntWidth(1)), size, 1, 0,
      Seq("read"), Seq("write"), Seq())
    val ref = WRef(name, BundleType(Seq(
      Field("read", Flip, BundleType(List(
        Field("addr", Default, UIntType(IntWidth(addrWidth))),
        Field("en",   Default, UIntType(IntWidth(1))),
        Field("clk",  Default, ClockType),
        Field("data", Flip,    UIntType(IntWidth(1)))
      ))),
      Field("write", Flip, BundleType(List(
        Field("addr", Default, UIntType(IntWidth(addrWidth))),
        Field("mask", Default, UIntType(IntWidth(1))),
        Field("en",   Default, UIntType(IntWidth(1))),
        Field("clk",  Default, ClockType),
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

class HierModuleInfoV10a(
  val mName: String,
  val insts: scala.collection.Set[WDefInstance],
  val regs:  scala.collection.Set[DefRegister])

object HierModuleInfoV10a {
  def apply(mod: DefModule, gLedger: graphLedger): HierModuleInfoV10a = {
    val insts = gLedger.getInstances
    val regs  = gLedger.findRegs
    new HierModuleInfoV10a(mod.name, insts, regs)
  }
}

class InstrHierCovV10a(
  mod:        DefModule,
  mInfo:      HierModuleInfoV10a,
  selection:  Option[ModuleSelection],
  extModules: Set[String],
  params:     HierCovParamsV10a) {
  import HierCovUtilV10a._

  private val mName = mod.name

  def instrument(): DefModule = mod match {
    case m: Module =>
      val stmts = m.body.asInstanceOf[Block].stmts
      val (clockName, hasClockFound) = hasClock(m)

      // If no selection exists for this module, we emit coverage ports wired
      // to zero — same behavior v9a exhibits when a module has no ctrl regs.
      val sel = selection.getOrElse(ModuleSelection())

      val inputBits = selectInputBitsFromAnno(m.ports, sel)
      val regBits   = selectRegBitsFromAnno(mInfo.regs, sel)

      val submodBits = mInfo.insts.filter(inst => !extModules.contains(inst.module)).flatMap { inst =>
        val ref = WSubField(WRef(inst), "io_hierCovHash")
        (0 until params.submodHashSize).map { idx =>
          val bit = bitExtract(ref, idx, UIntType(IntWidth(params.submodHashSize)))
          (bit, s"${inst.name}.io_hierCovHash[$idx]")
        }
      }.toSeq

      val coreBitsAll = regBits ++ submodBits

      var dynamicInputHashSize = if (inputBits.nonEmpty)   Math.min(params.maxInputHashSize, inputBits.size)   else 0
      var dynamicCoreHashSize  = if (coreBitsAll.nonEmpty) Math.min(params.maxCoreHashSize,  coreBitsAll.size) else 0

      if (dynamicInputHashSize + dynamicCoreHashSize > params.maxAddrWidth) {
        dynamicCoreHashSize = params.maxAddrWidth - dynamicInputHashSize
        if (dynamicCoreHashSize < 0) {
          dynamicInputHashSize = params.maxAddrWidth
          dynamicCoreHashSize  = 0
        }
      }

      val (inputHash, inputHashStmts) = buildDirectOrFold(inputBits,    dynamicInputHashSize, s"${mName}_in")
      val (coreHash,  coreHashStmts)  = buildDirectOrFold(coreBitsAll, dynamicCoreHashSize,  s"${mName}_core")

      val addrWidth = dynamicInputHashSize + dynamicCoreHashSize
      val addrExpr = DoPrim(Cat, Seq(inputHash, coreHash), Seq(), UIntType(IntWidth(addrWidth)))

      val covSumPort  = Port(NoInfo, "io_hierCovSum",  Output, UIntType(IntWidth(params.covSumSize)))
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

        val readSubField = WSubField(covRef, "read")
        val writeSubField = WSubField(covRef, "write")

        val rdAddr = Connect(NoInfo, WSubField(readSubField, "addr", UIntType(IntWidth(addrWidth)), SinkFlow), addrExpr)
        val rdEn   = Connect(NoInfo, WSubField(readSubField, "en",   UIntType(IntWidth(1)), SinkFlow), UIntLiteral(1, IntWidth(1)))
        val rdClk  = Connect(NoInfo, WSubField(readSubField, "clk",  ClockType, SinkFlow),               WRef(clockName, ClockType, PortKind))

        val wrAddr = Connect(NoInfo, WSubField(writeSubField, "addr", UIntType(IntWidth(addrWidth)), SinkFlow), addrExpr)
        val wrMask = Connect(NoInfo, WSubField(writeSubField, "mask", UIntType(IntWidth(1)), SinkFlow), UIntLiteral(1, IntWidth(1)))
        val wrEn   = Connect(NoInfo, WSubField(writeSubField, "en",   UIntType(IntWidth(1)), SinkFlow), UIntLiteral(1, IntWidth(1)))
        val wrClk  = Connect(NoInfo, WSubField(writeSubField, "clk",  ClockType, SinkFlow),               WRef(clockName, ClockType, PortKind))
        val wrData = Connect(NoInfo, WSubField(writeSubField, "data", UIntType(IntWidth(1))),            UIntLiteral(1, IntWidth(1)))

        val readData = WSubField(readSubField, "data", UIntType(IntWidth(1)))
        val newHit   = DoPrim(Not, Seq(readData), Seq(), UIntType(IntWidth(1)))

        val updateSum = Connect(NoInfo, WRef(covSum),
          Mux(readData,
            WRef(covSum),
            DoPrim(Add, Seq(WRef(covSum), UIntLiteral(1, IntWidth(1))), Seq(), UIntType(IntWidth(params.covSumSize)))
          ))

        val bucketIdx = xorFoldAddr(addrExpr, addrWidth, bucketIdxWidth)
        val bucketCons = bucketRegs.zipWithIndex.map {
          case (b, i) =>
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
            if (bucketBits.size < params.maxBucketSigBits) {
              bucketBits.append((bitExtract(WRef(b), idx, UIntType(IntWidth(width))), s"${mName}_covBucket_${i}[$idx]"))
            }
          }
        }

        val (covHash, covHashStmts) = buildDirectOrFold(bucketBits.toSeq, params.submodHashSize, s"${mName}_covHash")
        val covHashCon = Connect(NoInfo, WRef(covHashPort), covHash)

        val ports = m.ports ++ Seq(covSumPort, covHashPort)
        val newStmts = stmts ++ inputHashStmts ++ coreHashStmts ++ Seq(covMap, covSum) ++
          bucketRegs ++ Seq(rdAddr, rdEn, rdClk, wrAddr, wrMask, wrEn, wrClk, wrData, updateSum) ++
          bucketCons ++ Seq(covSumCon) ++ covHashStmts :+ covHashCon

        Module(m.info, mName, ports, Block(newStmts))
      } else {
        val covSumWire  = DefWire(NoInfo, s"${mName}_hierCovSum_wire",  UIntType(IntWidth(params.covSumSize)))
        val covHashWire = DefWire(NoInfo, s"${mName}_hierCovHash_wire", UIntType(IntWidth(params.submodHashSize)))
        val covSumZero  = Connect(NoInfo, WRef(covSumWire),  UIntLiteral(0, IntWidth(params.covSumSize)))
        val covHashZero = Connect(NoInfo, WRef(covHashWire), UIntLiteral(0, IntWidth(params.submodHashSize)))
        val covSumCon   = Connect(NoInfo, WRef(covSumPort),  WRef(covSumWire))
        val covHashCon  = Connect(NoInfo, WRef(covHashPort), WRef(covHashWire))

        val ports = m.ports ++ Seq(covSumPort, covHashPort)
        val newStmts = stmts ++ Seq(covSumWire, covHashWire, covSumZero, covHashZero, covSumCon, covHashCon)
        Module(m.info, mName, ports, Block(newStmts))
      }

    case ext: ExtModule => ext
    case other          => other
  }
}

// Assert + metaReset instrumentation: these are independent of bit selection,
// so we delegate to the V9a classes by re-instantiating them on a lightweight
// info wrapper. Copying the minimal logic here keeps v10a self-contained and
// avoids tight coupling to the V9a class contracts.

class InstrHierAssertV10a(mod: DefModule, insts: scala.collection.Set[WDefInstance]) {
  private val mName = mod.name

  def instrument(): DefModule = mod match {
    case m: Module =>
      val block = m.body
      val stmts = block.asInstanceOf[Block].stmts
      val (clockName, _, hasClockReset) = hasClockAndReset(m)

      val assertPort = Port(NoInfo, "metaAssert", Output, UIntType(IntWidth(1)))

      val stops = ListBuffer[Stop]()
      findStop(stops)(block)

      val stopEns = stops.zipWithIndex.map(t => DefNode(NoInfo, s"stopEn${t._2}", t._1.en)).toSeq
      val instAsserts = insts.map(inst =>
        (inst, DefWire(NoInfo, s"${inst.name}_metaAssert_wire", UIntType(IntWidth(1))))).toSeq
      val instAssertCons = instAsserts.map(t => Connect(NoInfo, WRef(t._2), WSubField(WRef(t._1), "metaAssert")))

      val (topOr, orStmts) = makeOr(stopEns.map(en => WRef(en)) ++ instAsserts.map(t => WRef(t._2)), 0)

      val conStmts =
        if (hasClockReset && topOr.nonEmpty) {
          val assertReg = DefRegister(NoInfo, s"${mName}_metaAssert", UIntType(IntWidth(1)),
            WRef(clockName, ClockType, PortKind, SourceFlow),
            UIntLiteral(0, IntWidth(1)),
            WRef(s"${mName}_metaAssert", UIntType(IntWidth(1)), RegKind, UnknownFlow))
          val or = DoPrim(Or, Seq(WRef(assertReg), topOr.get), Seq(), UIntType(IntWidth(1)))
          val assertRegCon = Connect(NoInfo, WRef(assertReg), or)
          val portCon = Connect(NoInfo, WRef(assertPort), WRef(assertReg))
          Seq[Statement](assertReg, assertRegCon, portCon)
        } else if (topOr.nonEmpty) {
          Seq[Statement](Connect(NoInfo, WRef(assertPort), topOr.get))
        } else {
          Seq[Statement](Connect(NoInfo, WRef(assertPort), UIntLiteral(0)))
        }

      val ports = m.ports :+ assertPort
      val newStmts = stmts ++ stopEns ++ instAsserts.map(t => t._2) ++ instAssertCons ++ orStmts ++ conStmts
      Module(m.info, mName, ports, Block(newStmts))

    case ext: ExtModule => ext
    case other          => other
  }

  private def makeOr(stopEns: Seq[WRef], id: Int): (Option[WRef], Seq[Statement]) = stopEns.length match {
    case 0 => (None, Seq[Statement]())
    case 1 => (Some(stopEns.head), Seq[Statement]())
    case 2 =>
      val orWire = DefWire(NoInfo, mName + s"_or${id}", UIntType(IntWidth(1)))
      val orOp = DoPrim(Or, stopEns, Seq(), UIntType(IntWidth(1)))
      val orCon = Connect(NoInfo, WRef(orWire), orOp)
      (Some(WRef(orWire)), Seq[Statement](orWire, orCon))
    case _ =>
      val (or1, s1) = makeOr(stopEns.splitAt(stopEns.length / 2)._1, 2 * id + 1)
      val (or2, s2) = makeOr(stopEns.splitAt(stopEns.length / 2)._2, 2 * id + 2)
      val orWire = DefWire(NoInfo, mName + s"_or${id}", UIntType(IntWidth(1)))
      val orOp = DoPrim(Or, Seq(or1.get, or2.get), Seq(), UIntType(IntWidth(1)))
      val orCon = Connect(NoInfo, WRef(orWire), orOp)
      (Some(WRef(orWire)), s1 ++ s2 :+ orWire :+ orCon)
  }

  private def findStop(stops: ListBuffer[Stop])(stmt: Statement): Unit = stmt match {
    case stop: Stop        => stops.append(stop)
    case s: Statement      => s foreachStmt findStop(stops)
  }

  private def hasClockAndReset(mod: Module): (String, String, Boolean) = {
    val ports = mod.ports
    val (clockName, resetName) = ports.foldLeft[(String, String)](("None", "None")) {
      (tuple, p) =>
        if (p.name == "clock") (p.name, tuple._2)
        else if (p.name contains "reset") (tuple._1, p.name)
        else tuple
    }
    (clockName, resetName, (clockName != "None") && (resetName != "None"))
  }
}

class InstrHierResetV10a(
  mod:         DefModule,
  insts:       scala.collection.Set[WDefInstance],
  regs:        scala.collection.Set[DefRegister],
  moduleInsts: scala.collection.Map[String, scala.collection.Set[WDefInstance]]) {
  private val mName = mod.name

  def instrument(): DefModule = mod match {
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

        moduleInsts.get(inst.module).foreach { childInsts =>
          for (childInst <- childInsts) {
            val haltRef = WSubField(WRef(inst), childInst.name + "_halt")
            childInstHaltCons.append(Connect(NoInfo, haltRef, UIntLiteral(0, IntWidth(1))))
          }
        }
      }

      val ports = (m.ports :+ metaResetPort) ++ instResetPorts
      Module(m.info, mName, ports, Block(newStmts ++ resetCons ++ childInstHaltCons))

    case ext: ExtModule => ext
    case other          => other
  }

  private def addMetaReset(metaReset: Port)(s: Statement): Statement = s match {
    case Connect(info, loc, expr) if regs.exists(r => r.name == loc.serialize) =>
      val reg = regs.find(r => r.name == loc.serialize).get
      reg.tpe match {
        case utpe: UIntType =>
          utpe.width match {
            case w: IntWidth => Connect(info, loc, Mux(WRef(metaReset), UIntLiteral(0, IntWidth(w.width)), expr))
            case _           => s
          }
        case stpe: SIntType =>
          stpe.width match {
            case w: IntWidth => Connect(info, loc, Mux(WRef(metaReset), SIntLiteral(0, IntWidth(w.width)), expr))
            case _           => s
          }
        case _ => s
      }
    case other => other.mapStmt(addMetaReset(metaReset))
  }
}

class hierCoverage_v10a extends Transform {
  def inputForm:  firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm
  def outputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm

  private val moduleInfos = mutable.Map[String, HierModuleInfoV10a]()
  private val params      = HierCovParamsV10a()

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit

    // Exactly-one-annotation contract. Missing → loud error.
    val selAnnos = state.annotations.collect { case a: HierCovSelectionAnnotation => a }
    val sel = selAnnos match {
      case Seq(s) => s
      case Seq()  =>
        throw new FirrtlUserException(
          "hierCoverage_v10a requires exactly one HierCovSelectionAnnotation; " +
          "run tools/hiercov_select/orchestrate.py to produce one, or use -fct hier_cov.hierCoverage_v9a.")
      case many   =>
        throw new FirrtlUserException(
          s"hierCoverage_v10a expected exactly one HierCovSelectionAnnotation but found ${many.size}.")
    }

    for (m <- circuit.modules) {
      val gLedger = new graphLedger(m)
      gLedger.parseModule
      moduleInfos(m.name) = HierModuleInfoV10a(m, gLedger)
    }

    val extModules = circuit.modules.filter(_.isInstanceOf[ExtModule]).map(_.name).toSet

    val instrCircuit = circuit map { m: DefModule =>
      val instrCov = new InstrHierCovV10a(m, moduleInfos(m.name), sel.modules.get(m.name), extModules, params)
      instrCov.instrument()
    }

    val assertCircuit = instrCircuit map { m: DefModule =>
      val instrAssert = new InstrHierAssertV10a(m, moduleInfos(m.name).insts)
      instrAssert.instrument()
    }

    val moduleInstsMap: scala.collection.Map[String, scala.collection.Set[WDefInstance]] =
      moduleInfos.map { case (k, v) => k -> v.insts }

    val metaResetCircuit = assertCircuit map { m: DefModule =>
      val mi = moduleInfos(m.name)
      val instrReset = new InstrHierResetV10a(m, mi.insts, mi.regs, moduleInstsMap)
      instrReset.instrument()
    }

    writeCoverageSummary(circuit, extModules, sel, metaResetCircuit.main)
    state.copy(metaResetCircuit)
  }

  private def writeCoverageSummary(
    circuit:    Circuit,
    extModules: Set[String],
    sel:        HierCovSelectionAnnotation,
    topName:    String): Unit = {

    val moduleMap = circuit.modules.map(m => m.name -> m).toMap
    val moduleNums: Map[String, Int] = moduleInfos.map { t => (t._1, findModules(topName, t._1)) }.toMap

    def covMapSizeOf(moduleName: String): Long = {
      moduleMap.get(moduleName) match {
        case Some(m: Module) =>
          val (_, hasClk) = HierCovUtilV10a.hasClock(m)
          if (!hasClk) 0L
          else {
            val mInfo = moduleInfos(moduleName)
            val selM = sel.modules.getOrElse(moduleName, ModuleSelection())
            val inputBits = HierCovUtilV10a.selectInputBitsFromAnno(m.ports, selM)
            val regBits   = HierCovUtilV10a.selectRegBitsFromAnno(mInfo.regs, selM)
            val submodInsts = mInfo.insts.count(inst => !extModules.contains(inst.module))
            val inputHashEff = if (inputBits.nonEmpty) Math.min(params.maxInputHashSize, inputBits.size) else 0
            val coreBitCount = regBits.size + submodInsts * params.submodHashSize
            val coreHashEff  = if (coreBitCount > 0) Math.min(params.maxCoreHashSize, coreBitCount) else 0
            var dynamicInput = inputHashEff
            var dynamicCore  = coreHashEff
            if (dynamicInput + dynamicCore > params.maxAddrWidth) {
              dynamicCore = params.maxAddrWidth - dynamicInput
              if (dynamicCore < 0) { dynamicInput = params.maxAddrWidth; dynamicCore = 0 }
            }
            val addrWidth = dynamicInput + dynamicCore
            if (addrWidth > 0) (1L << addrWidth) else 0L
          }
        case _ => 0L
      }
    }

    val perModule = moduleInfos.keys.toSeq.sorted.map { mName =>
      val covSize = covMapSizeOf(mName)
      val instCnt = moduleNums.getOrElse(mName, 0)
      val mInfo = moduleInfos(mName)
      val totalRegCount = mInfo.regs.size
      val selM = sel.modules.getOrElse(mName, ModuleSelection())
      val keptRegs = selM.regs.size
      val keptPorts = selM.ports.size
      s"  ${mName}: covMapSize=${covSize}, keptRegs=${keptRegs}, keptPorts=${keptPorts}, totalRegs=${totalRegCount}, instances=${instCnt}\n"
    }

    val totalCov = moduleInfos.keys.toSeq.foldLeft(0L) { (acc, mName) =>
      acc + covMapSizeOf(mName) * moduleNums.getOrElse(mName, 0).toLong
    }

    val text =
      s"Top module: ${topName}\n" +
      s"Total coverage points (hier_cov_v10a anno-driven): ${totalCov}\n" +
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
    else moduleInfos.get(topName)
      .map(_.insts.foldLeft(0)((num, inst) => num + findModules(inst.module, moduleName)))
      .getOrElse(0)
  }
}
