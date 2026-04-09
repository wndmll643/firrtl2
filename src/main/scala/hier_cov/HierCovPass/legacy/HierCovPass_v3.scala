package hier_cov

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._
import firrtl2.PrimOps._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import coverage.graphLedger

case class HierCovParamsV3(
  inputHashSize: Int = 6,
  coreHashSize: Int = 8,
  submodHashSize: Int = 6,
  covSumSize: Int = 32,
  maxInputPorts: Int = 8,
  maxBitsPerPort: Int = 8,
  maxRegBits: Int = 64
)

object HierCovUtilV3 {
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

  def selectInputBits(ports: Seq[Port], params: HierCovParamsV3): Seq[(Expression, String)] = {
    val dataInputs = ports.filter(p => p.direction == Input).filterNot { p =>
      val lname = p.name.toLowerCase
      p.tpe == ClockType || lname.contains("clock") || lname.contains("reset")
    }.filter(p => p.tpe.isInstanceOf[UIntType] || p.tpe.isInstanceOf[SIntType])

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

  def selectRegBits(regs: Seq[DefRegister], params: HierCovParamsV3): Seq[(Expression, String)] = {
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
}

class HierModuleInfoV3(val mName: String,
                     val insts: scala.collection.Set[WDefInstance],
                     val regs: scala.collection.Set[DefRegister])

object HierModuleInfoV3 {
  def apply(mod: DefModule, gLedger: graphLedger): HierModuleInfoV3 = {
    val insts = gLedger.getInstances
    val regs = gLedger.findRegs
    new HierModuleInfoV3(mod.name, insts, regs)
  }
}

class InstrHierCovV3(mod: DefModule,
                   mInfo: HierModuleInfoV3,
                   extModules: Set[String],
                   params: HierCovParamsV3) {
  import HierCovUtilV3._

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
          val covMapSize = Math.pow(2, addrWidth).toInt
          val (covMap, covRef) = defMemory(s"${mName}_hierCov", s"Hierarchical coverage map for ${mName}", covMapSize, addrWidth)
          val covSum = DefRegister(NoInfo, s"${mName}_hierCovSum", UIntType(IntWidth(params.covSumSize)),
            WRef(clockName, ClockType, PortKind, SourceFlow),
            UIntLiteral(0, IntWidth(1)),
            WRef(s"${mName}_hierCovSum", UIntType(IntWidth(params.covSumSize)), RegKind, UnknownFlow))

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

          val updateSum = Connect(NoInfo, WRef(covSum),
            Mux(WSubField(readSubField, "data", UIntType(IntWidth(1))),
              WRef(covSum),
              DoPrim(Add, Seq(WRef(covSum), UIntLiteral(1, IntWidth(1))), Seq(), UIntType(IntWidth(params.covSumSize)))
            ))

          val covSumCon = Connect(NoInfo, WRef(covSumPort), WRef(covSum))

          val covSumBits = (0 until params.covSumSize).map { idx =>
            (bitExtract(WRef(covSum), idx, UIntType(IntWidth(params.covSumSize))), s"${mName}_covSum[$idx]")
          }
          val (covHash, covHashStmts) = buildHash(covSumBits, params.submodHashSize, s"${mName}_covHash")
          val covHashCon = Connect(NoInfo, WRef(covHashPort), covHash)

          val ports = m.ports ++ Seq(covSumPort, covHashPort)
          val newStmts = stmts ++ inputHashStmts ++ coreHashStmts ++ Seq(covMap, covSum) ++
            Seq(rdAddr, rdEn, rdClk, wrAddr, wrMask, wrEn, wrClk, wrData, updateSum, covSumCon) ++
            covHashStmts :+ covHashCon

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

class InstrHierAssertV3(mod: DefModule, mInfo: HierModuleInfoV3) {
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

class InstrHierResetV3(mod: DefModule, mInfo: HierModuleInfoV3, moduleInfos: scala.collection.Map[String, HierModuleInfoV3]) {
  private val mName = mod.name
  private val insts = mInfo.insts
  private val regs = mInfo.regs

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
              case w: IntWidth => Connect(info, loc, Mux(WRef(metaReset), UIntLiteral(0, IntWidth(w.width)), expr))
              case _ => s
            }
          case _ => s
        }
      case other =>
        other.mapStmt(addMetaReset(metaReset))
    }
  }
}

class hierCoverage_v3 extends Transform {
  def inputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm
  def outputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm

  private val moduleInfos = mutable.Map[String, HierModuleInfoV3]()
  private val params = HierCovParamsV3()

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit

    for (m <- circuit.modules) {
      val gLedger = new graphLedger(m)
      gLedger.parseModule
      moduleInfos(m.name) = HierModuleInfoV3(m, gLedger)
    }

    val extModules = circuit.modules.filter(_.isInstanceOf[ExtModule]).map(_.name).toSet

    val instrCircuit = circuit map { m: DefModule =>
      val instrCov = new InstrHierCovV3(m, moduleInfos(m.name), extModules, params)
      instrCov.instrument()
    }

    val assertCircuit = instrCircuit map { m: DefModule =>
      val instrAssert = new InstrHierAssertV3(m, moduleInfos(m.name))
      instrAssert.instrument()
    }

    val metaResetCircuit = assertCircuit map { m: DefModule =>
      val instrReset = new InstrHierResetV3(m, moduleInfos(m.name), moduleInfos)
      instrReset.instrument()
    }

    state.copy(metaResetCircuit)
  }
}
