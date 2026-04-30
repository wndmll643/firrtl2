// metaReset wiring — shared across every hierCoverage_* variant.
//
// Each module gets a 1-bit `metaReset` input plus, for every direct child
// instance, an `<inst>_halt` input. The metaReset signal propagates as a
// gated reset onto every register's D-input via a Mux. Each child's
// `metaReset` port is driven by `metaReset OR <inst>_halt`. Grandchildren's
// `<inst>_halt` ports are driven low at this level, since each layer
// terminates them on the way down.
//
// Historically every variant declared `InstrHierResetV6a`, `V6b`, `V9a`,
// `V9b`, `V10a` with bit-for-bit identical bodies. This is the shared
// implementation (DUP-1 in the checklist).
//
// `moduleInsts` maps each module-name to the set of WDefInstances *inside*
// that module (used to drive grandchild halt ports). Pass the full
// `moduleInfos` map's `_.insts` projection.
package hier_cov

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._
import firrtl2.PrimOps._

import scala.collection.mutable.ListBuffer

class InstrHierReset(
  mod:         DefModule,
  insts:       scala.collection.Set[WDefInstance],
  regs:        scala.collection.Set[DefRegister],
  moduleInsts: scala.collection.Map[String, scala.collection.Set[WDefInstance]]
) {
  private val mName = mod.name

  def instrument(): DefModule = mod match {
    case m: Module =>
      val stmts          = m.body.asInstanceOf[Block].stmts
      val metaResetPort  = Port(NoInfo, "metaReset", Input, UIntType(IntWidth(1)))
      val newStmts       = stmts.map(addMetaReset(metaResetPort))

      val resetCons          = ListBuffer[Statement]()
      val instResetPorts     = ListBuffer[Port]()
      val childInstHaltCons  = ListBuffer[Statement]()

      for (inst <- insts) {
        val instResetPort = Port(NoInfo, inst.name + "_halt", Input, UIntType(IntWidth(1)))
        val instReset     = DoPrim(Or, Seq(WRef(metaResetPort), WRef(instResetPort)), Seq(), UIntType(IntWidth(1)))
        val instResetCon  = Connect(NoInfo, WSubField(WRef(inst), "metaReset"), instReset)

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

  /** Recursively walk the module body and replace any `Connect(loc, expr)`
    * targeting one of `regs` with `Connect(loc, Mux(metaReset, 0, expr))`.
    * SInt and UInt regs get correctly-typed zero literals. */
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
