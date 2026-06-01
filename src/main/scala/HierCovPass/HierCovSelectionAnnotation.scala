// Annotation carrying the per-module bit-selection decisions that drive
// hierCoverage_v10a. Produced by tools/hiercov_select/analyze.py and delivered
// via `-faf selection.json` on the firrtl2 CLI.
package hier_cov

import firrtl2.annotations.{HasSerializationHints, NoTargetAnnotation}

/** Per-module selection.
  *
  * @param regs  register name -> kept bit indices (LSB = 0)
  * @param ports port name    -> kept bit indices (LSB = 0)
  * @param muxes reserved for Phase 2 (ignored by v10a)
  */
case class ModuleSelection(
  regs:  Map[String, Seq[Int]] = Map.empty,
  ports: Map[String, Seq[Int]] = Map.empty,
  muxes: Seq[String]            = Seq.empty)

/** Top-level annotation for hierCoverage_v10a.
  *
  * Exactly one instance is expected in the FIRRTL annotation stream. v10a
  * errors out if missing or if multiple are found.
  */
case class HierCovSelectionAnnotation(
  version: Int,
  modules: Map[String, ModuleSelection])
    extends NoTargetAnnotation
    with HasSerializationHints {
  def typeHints: Seq[Class[_]] = Seq(classOf[ModuleSelection])
}
