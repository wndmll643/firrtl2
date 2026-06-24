// hier_cov.hierCoverage_data_bucket_tree_sorted — Gate-3-validation variant.
//
// Identical to data_bucket_tree EXCEPT it uses the sorted selector variants
// (`HierCovSelectors.select*Sorted`) so that the maxInputPorts/maxRegBits
// cap binds on an alphabetic-first subset rather than a JVM-HashSet-order
// subset. Used to validate bit-strict bucket-address equivalence against
// the CIRCT port's --hier-cov-data-bucket-tree-sorted variant.
//
// IMMUTABILITY: existing data_bucket_tree stays untouched; this is a NEW
// variant per the project policy.
package hier_cov

import java.io.{File, PrintWriter}

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._

import scala.collection.mutable

import coverage.graphLedger

import hier_cov.lib._

class hierCoverage_data_bucket_tree_sorted extends Transform {
  def inputForm:  firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm
  def outputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm

  private val moduleInfos = mutable.Map[String, HierModuleInfo]()
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
      val inputBits = HierCovSelectors.selectDataInputBitsSorted(ports, mInfo.ctrlPortNames, params)
      val regBits   = HierCovSelectors.selectControlRegBitsSorted(mInfo.ctrlRegs, mInfo.dirInRegs, params)
      new InstrHierCov(
        m, mInfo, extModules, params,
        inputBits, regBits, HierCovHash.bucketHash,
        emitSumTotal = true
      ).instrument()
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
    val moduleMapForSignals = circuit.modules.map(m => m.name -> m).toMap
    BucketManifest.maybeEmitWithSignals(
      metaResetCircuit, metaResetCircuit.main, moduleInfos, "data_bucket_tree_sorted",
      (mName: String) => moduleMapForSignals.get(mName) match {
        case Some(m: Module) =>
          val mInfo     = moduleInfos(mName)
          val inputBits = HierCovSelectors.selectDataInputBitsSorted(m.ports, mInfo.ctrlPortNames, params)
          val regBits   = HierCovSelectors.selectControlRegBitsSorted(mInfo.ctrlRegs, mInfo.dirInRegs, params)
          (inputBits.map(_._2), regBits.map(_._2))
        case _ => (Seq.empty[String], Seq.empty[String])
      }
    )
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
          val inputBits   = HierCovSelectors.selectDataInputBitsSorted(m.ports, mInfo.ctrlPortNames, params)
          val regBits     = HierCovSelectors.selectControlRegBitsSorted(mInfo.ctrlRegs, mInfo.dirInRegs, params)
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
      s"Total coverage points (hier_cov data_bucket_tree_sorted, data-input, tree-sum): ${totalCov}\n" +
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
