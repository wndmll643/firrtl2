// Patched regCoverage transform: identical to coverage.regCoverage but uses
// InstrResetPatch (SInt-safe metaReset injection) instead of InstrReset.

package coverage_patch

import java.io.{File, PrintWriter}

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._
import coverage.{moduleInfo, graphLedger, InstrCov, InstrAssert}

import scala.collection.mutable

class regCoverage_patch extends Transform {

  def inputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm
  def outputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm

  val moduleInfos = mutable.Map[String, moduleInfo]()
  var totNumOptRegs = 0

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit

    print("==================== Finding Control Registers ====================\n")
    for (m <- circuit.modules) {
      val gLedger = new graphLedger(m)
      gLedger.parseModule
      moduleInfos(m.name) = moduleInfo(m, gLedger)
      gLedger.printLog()
      print("done\n")
    }
    print("===================================================================\n")

    print("====================== Instrumenting Coverage =====================\n")
    val extModules = circuit.modules.filter(_.isInstanceOf[ExtModule]).map(_.name)
    val instrCircuit = circuit map { m: DefModule =>
      val instrCov = new InstrCov(m, moduleInfos(m.name), extModules)
      val mod = instrCov.instrument()
      totNumOptRegs = totNumOptRegs + instrCov.numOptRegs
      instrCov.printLog()
      moduleInfos(m.name).saveCovResult(instrCov)
      moduleInfos(m.name).printInfo()
      mod
    }
    print("===================================================================\n")

    print("====================== Instrumenting metaAssert ===================\n")
    val assertCircuit = instrCircuit map { m: DefModule =>
      val instrAssert = new InstrAssert(m, moduleInfos(m.name))
      instrAssert.instrument()
    }
    print("===================================================================\n")

    print("====================== Instrumenting MetaReset (patch) ============\n")
    val metaResetCircuit = assertCircuit map { m: DefModule =>
      val instrReset = new InstrResetPatch(m, moduleInfos(m.name), moduleInfos)
      val mod = instrReset.instrument()
      instrReset.printLog()
      mod
    }
    print("===================================================================\n")

    print("\n====================== Instrumentation Summary ==================\n")
    printSummary(circuit.main)
    print("===================================================================\n")

    val hierWriter = new PrintWriter(new File(s"${metaResetCircuit.main}_hierarchy.txt"))
    for ((mName, mInfo) <- moduleInfos) {
      val insts = mInfo.insts
      hierWriter.write(s"$mName\t${insts.size}\t${mInfo.covSize}\n")
      insts.map(inst => hierWriter.write(s"\t${inst.module}\t${inst.name}\n"))
    }
    hierWriter.close()

    state.copy(metaResetCircuit)
  }

  def printSummary(topName: String): Unit = {
    assert(moduleInfos.size > 0, "printSummary must be called after instrumentation\n")

    val moduleNums = moduleInfos.map(tuple => (tuple._1, findModules(topName, tuple._1))).toMap

    val totRegNum = moduleInfos.foldLeft(0)((t, x) => t + (x._2.regNum * moduleNums(x._1)))
    val totCtrlRegNum = moduleInfos.foldLeft(0)((t, x) => t + (x._2.ctrlRegNum * moduleNums(x._1)))
    val totMuxNum = moduleInfos.foldLeft(0)((t, x) => t + (x._2.muxNum * moduleNums(x._1)))
    val totRegBitWidth = moduleInfos.foldLeft(0)((t, x) => t + (x._2.regBitWidth * moduleNums(x._1)))
    val totCtrlRegBitWidth = moduleInfos.foldLeft(0)((t, x) => t + (x._2.ctrlRegBitWidth * moduleNums(x._1)))
    val totCtrlBitWidth_optimized = moduleInfos.foldLeft(0)((t, x) => t + (x._2.ctrlBitWidth * moduleNums(x._1)))
    val totMuxBitWidth = totMuxNum
    val totMuxCtrlBitWidth = moduleInfos.foldLeft(0)((t, x) => t + (x._2.muxCtrlNum * moduleNums(x._1)))

    print(s"Total number of registers: ${totRegNum}\n" +
      s"Total number of control registers: ${totCtrlRegNum}\n" +
      s"Total number of muxes: ${totMuxNum}\n" +
      s"Total number of optimized registers: ${totNumOptRegs}\n" +
      s"Total bit width of registers: ${totRegBitWidth}\n" +
      s"Total bit width of control registers: ${totCtrlRegBitWidth}\n" +
      s"Optimized total bit width of control registers: ${totCtrlBitWidth_optimized}\n" +
      s"Total bit width of muxes: ${totMuxBitWidth}\n" +
      s"Total bit width of muxes: ${totMuxCtrlBitWidth}\n"
    )
  }

  def findModules(topName: String, moduleName: String): Int = {
    if (topName == moduleName) 1
    else {
      moduleInfos(topName).insts.foldLeft(0)((num, inst) => {
        num + findModules(inst.module, moduleName)
      })
    }
  }
}
