// hier_cov.hierCoverage_v10a — data-driven selection variant.
//
// Selection : per-module reg/port bit indices supplied externally via
//             HierCovSelectionAnnotation (loaded from selection.json by
//             FIRRTL's `-faf` flag). Produced upstream by
//             tools/hiercov_select/analyze.py from a calibration VCD.
// Hashing   : direct-or-fold (same as v9a downstream of selection)
// Sizing    : same as v9a (HierCovParams defaults)
// Hash pipe : 16-bit io_hierCovHash to parents (same as v9a)
//
// The intentional contract is that v10a is byte-for-byte v9a downstream of
// the selection step — any TTB delta vs v9a is therefore attributable to
// the selection change alone (V10-1 mostly resolved via lib refactor).
//
// Refactored to call into hier_cov.lib (DUP-1, V10-1 in checklist).
package hier_cov

import java.io.{File, PrintWriter}

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._

import scala.collection.mutable

import coverage.graphLedger

class hierCoverage_v10a extends Transform {
  def inputForm:  firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm
  def outputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm

  private val moduleInfos = mutable.Map[String, HierModuleInfo]()
  private val params      = HierCovParams()    // shared defaults; v10a tweaks none

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit

    // Exactly-one HierCovSelectionAnnotation contract — no fallback.
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
      // v10a doesn't need findMuxSrcs (the annotation already specifies what to keep).
      // Use the slim info — saves the O(N²) reverseEdge walk in graphLedger
      // (resolves part of GL-1's cost for the v10a path; full fix is in graphLedger itself).
      moduleInfos(m.name) = HierModuleInfo.slim(m, gLedger)
    }

    val extModules = circuit.modules.filter(_.isInstanceOf[ExtModule]).map(_.name).toSet

    val instrCircuit = circuit map { m: DefModule =>
      val mInfo = moduleInfos(m.name)
      val modSel = sel.modules.getOrElse(m.name, ModuleSelection())
      val (inputBits, regBits) = HierCovSelectors.selectFromAnnotation(
        keptPorts   = modSel.ports,
        keptRegs    = modSel.regs,
        modulePorts = m match { case mm: Module => mm.ports; case _ => Seq.empty },
        moduleRegs  = mInfo.regs
      )
      new InstrHierCov(m, mInfo, extModules, params, inputBits, regBits, HierCovHash.directOrFold).instrument()
    }

    val assertCircuit = instrCircuit map { m: DefModule =>
      new InstrHierAssert(m, moduleInfos(m.name).insts).instrument()
    }

    val moduleInstsMap: scala.collection.Map[String, scala.collection.Set[WDefInstance]] =
      moduleInfos.map { case (k, v) => k -> v.insts }

    val metaResetCircuit = assertCircuit map { m: DefModule =>
      val mi = moduleInfos(m.name)
      new InstrHierReset(m, mi.insts, mi.regs, moduleInstsMap).instrument()
    }

    writeCoverageSummary(circuit, sel, metaResetCircuit.main)
    state.copy(metaResetCircuit)
  }

  /** Per-top summary file — reports the actual annotation-driven selection
    * counts so it's easy to verify after a build that the right selection
    * landed. Same CWD-clobber caveat as v9a (CS-1, CS-2). */
  private def writeCoverageSummary(circuit: Circuit, sel: HierCovSelectionAnnotation, topName: String): Unit = {
    val moduleMap = circuit.modules.map(m => m.name -> m).toMap
    val moduleNums: Map[String, Int] = moduleInfos.map { t => (t._1, findModules(topName, t._1)) }.toMap

    val perModule = moduleInfos.keys.toSeq.sorted.map { mName =>
      val mInfo  = moduleInfos(mName)
      val instCnt = moduleNums.getOrElse(mName, 0)
      val totalRegCount = mInfo.regs.size
      val selM   = sel.modules.getOrElse(mName, ModuleSelection())
      val keptRegs  = selM.regs.size
      val keptPorts = selM.ports.size
      s"  ${mName}: keptRegs=${keptRegs}, keptPorts=${keptPorts}, totalRegs=${totalRegCount}, instances=${instCnt}\n"
    }

    val text =
      s"Top module: ${topName}\n" +
      s"Coverage variant: hier_cov_v10a (annotation-driven selection)\n" +
      "Per-module selection:\n" +
      perModule.mkString("")

    val named = new PrintWriter(new File(s"${topName}_hier_cov_summary.txt"))
    named.write(text); named.close()
    val compat = new PrintWriter(new File("summary.txt"))
    compat.write(text); compat.close()
  }

  private def findModules(topName: String, moduleName: String): Int =
    if (topName == moduleName) 1
    else moduleInfos.get(topName)
      .map(_.insts.foldLeft(0)((num, inst) => num + findModules(inst.module, moduleName)))
      .getOrElse(0)
}
