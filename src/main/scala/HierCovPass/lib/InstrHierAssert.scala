// metaAssert wiring — shared across every hierCoverage_* variant.
//
// Each module gets a 1-bit `metaAssert` output that is the registered OR of
// (a) every `Stop` enable inside the module body and (b) every child
// instance's own `metaAssert` port. The output sticks at 1 once any source
// has fired (since the OR feeds back through a register), so the host can
// poll for "any assertion has occurred under this subtree" with a single
// read.
//
// Historically every variant declared `InstrHierAssertV6a`, `V6b`, `V9a`,
// `V9b`, `V10a` with bit-for-bit identical bodies. This is the shared
// implementation (DUP-1 in the checklist).
package hier_cov

import firrtl2._
import firrtl2.ir._
import firrtl2.PrimOps._

import scala.collection.mutable.ListBuffer

class InstrHierAssert(mod: DefModule, insts: scala.collection.Set[WDefInstance]) {
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

  /** Build a balanced-by-half OR tree across the supplied 1-bit references.
    * Returns (Some(top-of-tree-WRef), supporting wires/connects) — or (None,
    * empty) when the input list is empty. */
  private def makeOr(stopEns: Seq[WRef], id: Int): (Option[WRef], Seq[Statement]) = stopEns.length match {
    case 0 => (None, Seq[Statement]())
    case 1 => (Some(stopEns.head), Seq[Statement]())
    case 2 =>
      val orWire = DefWire(NoInfo, mName + s"_or${id}", UIntType(IntWidth(1)))
      val orOp   = DoPrim(Or, stopEns, Seq(), UIntType(IntWidth(1)))
      val orCon  = Connect(NoInfo, WRef(orWire), orOp)
      (Some(WRef(orWire)), Seq[Statement](orWire, orCon))
    case _ =>
      val (or1, s1) = makeOr(stopEns.splitAt(stopEns.length / 2)._1, 2 * id + 1)
      val (or2, s2) = makeOr(stopEns.splitAt(stopEns.length / 2)._2, 2 * id + 2)
      val orWire = DefWire(NoInfo, mName + s"_or${id}", UIntType(IntWidth(1)))
      val orOp   = DoPrim(Or, Seq(or1.get, or2.get), Seq(), UIntType(IntWidth(1)))
      val orCon  = Connect(NoInfo, WRef(orWire), orOp)
      (Some(WRef(orWire)), s1 ++ s2 :+ orWire :+ orCon)
  }

  private def findStop(stops: ListBuffer[Stop])(stmt: Statement): Unit = stmt match {
    case stop: Stop   => stops.append(stop)
    case s: Statement => s foreachStmt findStop(stops)
  }

  /** Locate the conventional clock + reset port pair on this module. Returns
    * ("None", "None", false) if either is missing. Same heuristic as the
    * historical per-variant copies (V9-11 in the checklist — fragile against
    * `gated_clock`, `rst_n`, `aresetn`, but kept identical for now). */
  private def hasClockAndReset(mod: Module): (String, String, Boolean) = {
    val ports = mod.ports
    val (clockName, resetName) = ports.foldLeft[(String, String)](("None", "None")) { (tuple, p) =>
      if (p.name == "clock") (p.name, tuple._2)
      else if (p.name contains "reset") (tuple._1, p.name)
      else tuple
    }
    (clockName, resetName, (clockName != "None") && (resetName != "None"))
  }
}
