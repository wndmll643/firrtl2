// hier_cov.hierCoverage_v6a — DifuzzRTL-style data-input baseline.
//
// Selection : data input ports (non-control) + control regs (mux-source-derived)
// Hashing   : bucket-XOR-reduce
// Sizing    : min(numBits, cap), no maxAddrWidth ceiling (cap is small)
// Hash pipe : 6-bit io_hierCovHash to parents
//
// Used by encarsia/hierfuzz Yosys flow as the canonical DifuzzRTL-data-input
// baseline. Refactored to call into hier_cov.lib (DUP-1).
package hier_cov

import java.io.{File, PrintWriter}

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._

import scala.collection.mutable

import coverage.graphLedger

// Shared infra (HierModuleInfo, HierCovParams, HierCovSelectors,
// HierCovHash, InstrHierCov, InstrHierAssert, InstrHierReset) lives in
// `hier_cov.lib` to avoid collisions with the unsuffixed legacy/v1
// classes that share the parent `hier_cov` package.
import hier_cov.lib._

class hierCoverage_v6a extends Transform {
  def inputForm:  firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm
  def outputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm

  private val moduleInfos = mutable.Map[String, HierModuleInfo]()
  // v6a-specific knobs: smaller hash widths than v9a, no maxAddrWidth ceiling.
  // (Set maxAddrWidth larger than max(maxInputHashSize+maxCoreHashSize) so it
  // never trips — preserves v6a's no-cap behaviour.)
  private val params = HierCovParams(
    maxInputHashSize = 6,
    maxCoreHashSize  = 6,
    maxAddrWidth     = 12,
    submodHashSize   = 6
  )

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
      val ports     = m match { case mm: Module => mm.ports; case _ => Seq.empty[Port] }
      val inputBits = HierCovSelectors.selectDataInputBits(ports, mInfo.ctrlPortNames, params)
      val regBits   = HierCovSelectors.selectControlRegBits(mInfo.ctrlRegs, mInfo.dirInRegs, params)
      new InstrHierCov(m, mInfo, extModules, params, inputBits, regBits, HierCovHash.bucketHash).instrument()
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

  private def writeCoverageSummary(circuit: Circuit, extModules: Set[String], topName: String): Unit = {
    val moduleMap = circuit.modules.map(m => m.name -> m).toMap
    val moduleNums: Map[String, Int] = moduleInfos.map { t => (t._1, findModules(topName, t._1)) }.toMap

    def covMapSizeOf(moduleName: String): Long = moduleMap.get(moduleName) match {
      case Some(m: Module) =>
        val (_, hasClk) = HierCovUtil.hasClock(m)
        if (!hasClk) 0L
        else {
          val mInfo       = moduleInfos(moduleName)
          val inputBits   = HierCovSelectors.selectDataInputBits(m.ports, mInfo.ctrlPortNames, params)
          val regBits     = HierCovSelectors.selectControlRegBits(mInfo.ctrlRegs, mInfo.dirInRegs, params)
          val submodInsts = mInfo.insts.count(inst => !extModules.contains(inst.module))
          val ih          = if (inputBits.nonEmpty) Math.min(params.maxInputHashSize, inputBits.size) else 0
          val coreBitCount = regBits.size + submodInsts * params.submodHashSize
          val ch          = if (coreBitCount > 0) Math.min(params.maxCoreHashSize, coreBitCount) else 0
          val addrWidth   = ih + ch
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
      s"  ${mName}: covMapSize=${covSize}, ctrlRegs=${ctrlRegCount}, totalRegs=${totalRegCount}, instances=${instCnt}\n"
    }
    val totalCov = moduleInfos.keys.toSeq.foldLeft(0L) { (acc, mName) =>
      acc + covMapSizeOf(mName) * moduleNums.getOrElse(mName, 0).toLong
    }

    val text =
      s"Top module: ${topName}\n" +
      s"Total coverage points (hier_cov_v6a data-input): ${totalCov}\n" +
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
