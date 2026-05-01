// hier_cov.hierCoverage_v9a — production default.
//
// Selection : control inputs + control regs (mux-source-derived)
// Hashing   : direct-or-fold (zero-loss when bits ≤ outWidth, XOR-fold otherwise)
// Sizing    : min(numBits, cap), capped at maxAddrWidth=20 (1M map / module)
// Hash pipe : 16-bit io_hierCovHash to parents
//
// Refactored to call into hier_cov.lib (DUP-1 in naive_design_checklist.md).
// Behavioural contract preserved: this pass produces the same Verilog as
// the pre-refactor version on the same input. The pre-refactor body
// (~660 lines) is reduced to the variant-specific selection wiring + the
// summary writer.
package hier_cov

import java.io.{File, PrintWriter}

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._

import scala.collection.mutable

import coverage.graphLedger

// Shared infra lives in `hier_cov.lib` (avoids collision with legacy/v1).
import hier_cov.lib._

class hierCoverage_v9a extends Transform {
  def inputForm:  firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm
  def outputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm

  private val moduleInfos = mutable.Map[String, HierModuleInfo]()
  private val params      = HierCovParams()    // v9a defaults

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit

    for (m <- circuit.modules) {
      val gLedger = new graphLedger(m)
      gLedger.parseModule
      moduleInfos(m.name) = HierModuleInfo(m, gLedger)
    }

    val extModules = circuit.modules.filter(_.isInstanceOf[ExtModule]).map(_.name).toSet

    val instrCircuit = circuit map { m: DefModule =>
      val mInfo     = moduleInfos(m.name)
      val inputBits = HierCovSelectors.selectControlInputBits(m match {
        case mm: Module => mm.ports
        case _          => Seq.empty
      }, mInfo.ctrlPortNames, params)
      val regBits   = HierCovSelectors.selectControlRegBits(mInfo.ctrlRegs, mInfo.dirInRegs, params)
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

    writeCoverageSummary(circuit, extModules, metaResetCircuit.main)
    state.copy(metaResetCircuit)
  }

  /** Per-top summary file. CS-1/CS-2 in the checklist: writes to CWD with
    * no error handling and a hard-coded `summary.txt` second filename.
    * Kept identical for now to preserve existing automation that scrapes
    * these files. */
  private def writeCoverageSummary(circuit: Circuit, extModules: Set[String], topName: String): Unit = {
    val moduleMap = circuit.modules.map(m => m.name -> m).toMap
    val moduleNums: Map[String, Int] = moduleInfos.map { t => (t._1, findModules(topName, t._1)) }.toMap

    def covMapSizeOf(moduleName: String): Long = moduleMap.get(moduleName) match {
      case Some(m: Module) =>
        val (_, hasClk) = HierCovUtil.hasClock(m)
        if (!hasClk) 0L
        else {
          val mInfo     = moduleInfos(moduleName)
          val inputBits = HierCovSelectors.selectControlInputBits(m.ports, mInfo.ctrlPortNames, params)
          val regBits   = HierCovSelectors.selectControlRegBits(mInfo.ctrlRegs, mInfo.dirInRegs, params)
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

    val perModule = moduleInfos.keys.toSeq.sorted.map { mName =>
      val covSize       = covMapSizeOf(mName)
      val instCnt       = moduleNums.getOrElse(mName, 0)
      val mInfo         = moduleInfos(mName)
      val ctrlRegCount  = mInfo.ctrlRegs.size
      val totalRegCount = mInfo.regs.size
      val (inputHashH, coreHashH) = moduleMap.get(mName) match {
        case Some(m: Module) =>
          val inputBits   = HierCovSelectors.selectControlInputBits(m.ports, mInfo.ctrlPortNames, params)
          val regBits     = HierCovSelectors.selectControlRegBits(mInfo.ctrlRegs, mInfo.dirInRegs, params)
          val submodInsts = mInfo.insts.count(inst => !extModules.contains(inst.module))
          val ih = if (inputBits.nonEmpty) Math.min(params.maxInputHashSize, inputBits.size) else 0
          val coreBitCount = regBits.size + submodInsts * params.submodHashSize
          val ch = if (coreBitCount > 0) Math.min(params.maxCoreHashSize, coreBitCount) else 0
          var dynIh = ih
          var dynCh = ch
          if (dynIh + dynCh > params.maxAddrWidth) {
            dynCh = params.maxAddrWidth - dynIh
            if (dynCh < 0) { dynIh = params.maxAddrWidth; dynCh = 0 }
          }
          (dynIh, dynCh)
        case _ => (0, 0)
      }
      s"  ${mName}: covMapSize=${covSize}, inputHash=${inputHashH}, coreHash=${coreHashH}, ctrlRegs=${ctrlRegCount}, totalRegs=${totalRegCount}, instances=${instCnt}\n"
    }
    val totalCov = moduleInfos.keys.toSeq.foldLeft(0L) { (acc, mName) =>
      acc + covMapSizeOf(mName) * moduleNums.getOrElse(mName, 0).toLong
    }

    val text =
      s"Top module: ${topName}\n" +
      s"Total coverage points (hier_cov_v9a ctrl-input): ${totalCov}\n" +
      "Per-module coverage points:\n" +
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
