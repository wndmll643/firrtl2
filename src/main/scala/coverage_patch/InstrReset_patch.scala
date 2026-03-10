// Patched version of InstrReset with two fixes:
// 1. Uses SIntLiteral(0) instead of UIntLiteral(0) for SInt-typed registers,
//    fixing the FIRRTL type-mismatch error on designs like FPU / DivSqrtRawFN.
// 2. Drives each instance's child _halt ports to 0 (like InstrHierResetV4),
//    preventing RefNotInitializedException on deeply-nested module hierarchies.

package coverage_patch

import firrtl2.PrimOps.Or
import firrtl2._
import firrtl2.ir._
import coverage.moduleInfo

import scala.collection.mutable.ListBuffer

class InstrResetPatch(mod: DefModule,
                      mInfo: moduleInfo,
                      moduleInfos: scala.collection.Map[String, moduleInfo]) {
  private val mName = mod.name
  private val insts = mInfo.insts
  private val regs = if (mInfo.assertReg != None) (mInfo.regs.toSeq :+ mInfo.assertReg.get).toSet else mInfo.regs

  def printLog(): Unit = {
    print("=============================================\n")
    print(s"${mName}\n")
    print("---------------------------------------------\n")
    insts.foreach(inst => print(s"- [${inst.module}]: ${inst.name}\n"))
    print("=============================================\n")
  }

  def instrument(): DefModule = {
    mod match {
      case mod: Module =>
        val stmts = mod.body.asInstanceOf[Block].stmts
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

          // Drive child-instance _halt ports to 0 so they are fully initialized.
          moduleInfos.get(inst.module).foreach { childInfo =>
            for (childInst <- childInfo.insts) {
              val haltRef = WSubField(WRef(inst), childInst.name + "_halt")
              childInstHaltCons.append(Connect(NoInfo, haltRef, UIntLiteral(0, IntWidth(1))))
            }
          }
        }

        val ports = (mod.ports :+ metaResetPort) ++ instResetPorts
        val newBlock = Block(newStmts ++ resetCons ++ childInstHaltCons)

        Module(mod.info, mName, ports, newBlock)

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
        val zeroLit = reg.tpe match {
          case utpe: UIntType => UIntLiteral(0, IntWidth(utpe.width.asInstanceOf[IntWidth].width))
          case stpe: SIntType => SIntLiteral(0, IntWidth(stpe.width.asInstanceOf[IntWidth].width))
          case _ => throw new Exception("Register type must be one of UInt/SInt")
        }
        Connect(info, loc, Mux(WRef(metaReset), zeroLit, expr))
      case other =>
        other.mapStmt(addMetaReset(metaReset))
    }
  }
}
