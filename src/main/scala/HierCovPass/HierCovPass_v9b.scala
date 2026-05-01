// hier_cov.hierCoverage_v9b — maximum-coverage variant.
//
// Selection : control input ports + control regs + ExtModule input-port proxy
// Hashing   : direct-or-fold (same as v9a)
// Sizing    : min(numBits, cap) (the minHashSize floor in the original
//             collapses to min(cap, n) for v9b's parameter values)
// Hash pipe : 16-bit io_hierCovHash to parents
// Notable   : 32-way bucket histogram (vs v9a's 16), 256 maxRegBits (vs 64),
//             extmodule input-port proxy folded into core hash.
//
// Highest absolute coverage; not the fastest TTB. Refactored to call into
// hier_cov.lib (DUP-1).
package hier_cov

import java.io.{File, PrintWriter}

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._

import scala.collection.mutable

import coverage.graphLedger

// Shared infra lives in `hier_cov.lib` (avoids collision with legacy/v1).
import hier_cov.lib._

class hierCoverage_v9b extends Transform {
  def inputForm:  firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm
  def outputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm

  private val moduleInfos = mutable.Map[String, HierModuleInfo]()
  // v9b knobs: wider input cap, 256 reg-bit budget, 32-way bucket histogram.
  private val params = HierCovParams(
    maxInputHashSize = 10,
    maxCoreHashSize  = 14,
    maxAddrWidth     = 20,
    submodHashSize   = 16,
    maxRegBits       = 256,
    bucketCount      = 32,
    maxBucketSigBits = 256
  )
  // ExtModule proxy knobs (not in HierCovParams since they're v9b-only).
  private val maxExtModPorts       = 16
  private val maxExtModBitsPerPort = 8

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit

    for (m <- circuit.modules) {
      val gLedger = new graphLedger(m)
      gLedger.parseModule
      moduleInfos(m.name) = HierModuleInfo(m, gLedger)
    }

    val extModules     = circuit.modules.filter(_.isInstanceOf[ExtModule]).map(_.name).toSet
    val extModulePorts = HierCovSelectors.collectExtModulePorts(circuit)

    val instrCircuit = circuit map { m: DefModule =>
      val mInfo     = moduleInfos(m.name)
      val ports     = m match { case mm: Module => mm.ports; case _ => Seq.empty[Port] }
      val inputBits = HierCovSelectors.selectControlInputBits(ports, mInfo.ctrlPortNames, params)
      val regBits   = HierCovSelectors.selectControlRegBits(mInfo.ctrlRegs, mInfo.dirInRegs, params)
      val proxyBits = HierCovSelectors.selectExtModuleProxyBits(
        mInfo.insts, extModules, extModulePorts, maxExtModPorts, maxExtModBitsPerPort)
      new InstrHierCov(
        mod          = m,
        mInfo        = mInfo,
        extModules   = extModules,
        params       = params,
        inputBits    = inputBits,
        regBits      = regBits,
        hashFn       = HierCovHash.directOrFold,
        extraCoreBits = proxyBits
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

    writeCoverageSummary(circuit, extModules, extModulePorts, metaResetCircuit.main)
    state.copy(metaResetCircuit)
  }

  private def writeCoverageSummary(
    circuit:        Circuit,
    extModules:     Set[String],
    extModulePorts: scala.collection.Map[String, Seq[Port]],
    topName:        String
  ): Unit = {
    val moduleMap = circuit.modules.map(m => m.name -> m).toMap
    val moduleNums: Map[String, Int] = moduleInfos.map { t => (t._1, findModules(topName, t._1)) }.toMap

    def covMapSizeOf(moduleName: String): Long = moduleMap.get(moduleName) match {
      case Some(m: Module) =>
        val (_, hasClk) = HierCovUtil.hasClock(m)
        if (!hasClk) 0L
        else {
          val mInfo       = moduleInfos(moduleName)
          val inputBits   = HierCovSelectors.selectControlInputBits(m.ports, mInfo.ctrlPortNames, params)
          val regBits     = HierCovSelectors.selectControlRegBits(mInfo.ctrlRegs, mInfo.dirInRegs, params)
          val proxyBits   = HierCovSelectors.selectExtModuleProxyBits(
            mInfo.insts, extModules, extModulePorts, maxExtModPorts, maxExtModBitsPerPort)
          val submodInsts = mInfo.insts.count(inst => !extModules.contains(inst.module))
          val ih          = if (inputBits.nonEmpty) Math.min(params.maxInputHashSize, inputBits.size) else 0
          val coreBitCount = regBits.size + submodInsts * params.submodHashSize + proxyBits.size
          val ch          = if (coreBitCount > 0) Math.min(params.maxCoreHashSize, coreBitCount) else 0
          var dynIh = ih
          var dynCh = ch
          if (dynIh + dynCh > params.maxAddrWidth) {
            dynCh = params.maxAddrWidth - dynIh
            if (dynCh < 0) { dynIh = params.maxAddrWidth; dynCh = 0 }
          }
          val addrWidth = dynIh + dynCh
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
      s"Total coverage points (hier_cov_v9b ctrl-input + extmod-proxy): ${totalCov}\n" +
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
