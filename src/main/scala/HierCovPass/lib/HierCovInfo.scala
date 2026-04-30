// Per-module information collected once during pass execution and reused by
// the instrumentation, assert, and reset stages. Each variant historically
// declared its own near-identical `HierModuleInfo*` class; this is the shared
// version (DUP-2 in the checklist).
//
// All fields are `scala.collection.Set` rather than `Set` to remain
// compatible with the graphLedger API surface that returns the immutable
// flavour.
package hier_cov

import firrtl2.ir._
import coverage.graphLedger

class HierModuleInfo(
  val mName:          String,
  val insts:          scala.collection.Set[WDefInstance],
  val regs:           scala.collection.Set[DefRegister],   // ALL regs — needed for metaReset
  val ctrlRegs:       scala.collection.Set[DefRegister],   // mux-source regs only (may be empty for v10a)
  val dirInRegs:      scala.collection.Set[DefRegister],   // directly-input-fed control regs
  val ctrlPortNames:  Set[String]                          // input ports feeding mux conditions
)

object HierModuleInfo {

  /** Build the full info from a parsed graphLedger. Computes `findMuxSrcs`
    * (which populates the reverseEdge graph) before `findDirInRegs` since the
    * latter requires the former. */
  def apply(mod: DefModule, gLedger: graphLedger): HierModuleInfo = {
    val insts      = gLedger.getInstances
    val regs       = gLedger.findRegs
    val ctrlSrcs   = gLedger.findMuxSrcs           // must run before findDirInRegs
    val dirInRegs  = gLedger.findDirInRegs
    val ctrlRegs   = ctrlSrcs("DefRegister").map(_.node.asInstanceOf[DefRegister]).toSet
    val ctrlPortNames = ctrlSrcs("Port").map(_.name).toSet
    new HierModuleInfo(mod.name, insts, regs, ctrlRegs, dirInRegs, ctrlPortNames)
  }

  /** Slim variant for v10a, which doesn't need `findMuxSrcs`-derived sets
    * (the selection annotation already specifies which regs/ports to keep). */
  def slim(mod: DefModule, gLedger: graphLedger): HierModuleInfo = {
    val insts = gLedger.getInstances
    val regs  = gLedger.findRegs
    new HierModuleInfo(mod.name, insts, regs, Set.empty, Set.empty, Set.empty)
  }
}
