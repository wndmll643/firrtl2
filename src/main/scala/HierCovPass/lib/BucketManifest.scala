// Bucket manifest emission for the bitmap-mode (v2) fuzzer pipeline.
//
// Goal: produce a JSON side-file alongside the instrumented Verilog that
// lists every instrumented submodule instance and its coverage-map size.
// The fuzzer Python harness reads this manifest at startup and uses each
// `path` to walk into the cocotb DUT (BoomTile) and read the per-bucket
// `_hierCov` memory contents via cocotb's internal-signal access
// (enabled by Verilator's `--public-flat-rw` build flag).
//
// IMMUTABILITY contract: this helper produces ONLY a JSON side file. It
// does not modify the FIRRTL IR. Existing variant passes that wire in
// `BucketManifest.maybeEmit` produce bit-identical Verilog regardless of
// whether the manifest gets written — emission is gated on the env var
// `HIERCOV_EMIT_MANIFEST=1`, so absence of the env var = no side file =
// the original pass behavior.
//
// Manifest shape:
//
// {
//   "topModule": "TestHarness",
//   "variant":   "data_bucket",
//   "totalInstances": 489,
//   "totalBuckets":   1572864,
//   "instances": [
//     {"path": "",                       "moduleName": "TestHarness", "bucketCount": 4096, "memName": "TestHarness_hierCov"},
//     {"path": "tile_BOOM",              "moduleName": "BoomTile",    "bucketCount": 4096, "memName": "BoomTile_hierCov"},
//     {"path": "tile_BOOM.core",         "moduleName": "BoomCore",    "bucketCount": 4096, "memName": "BoomCore_hierCov"},
//     ...
//   ]
// }
//
// Path semantics: instance paths are RELATIVE to the FIRRTL top module
// (`circuit.main`). The Python side knows the cocotb DUT is BoomTile and
// filters/strips accordingly. An empty path "" denotes the top module
// itself. Each segment is an instance name (NOT a module name) so the
// path can be fed directly into cocotb's `walk_dotted(dut, path)`.
//
// Bucket count = 2^addrWidth for the module, identical to the
// `covMapSizeOf` logic in the variant pass's `writeCoverageSummary`. We
// re-derive it from the post-instrumentation `DefMemory.depth` to avoid
// duplicating selector logic per variant.
package hier_cov.lib

import java.io.{File, PrintWriter}

import firrtl2.ir._

object BucketManifest {

  case class InstancePath(path: String, moduleName: String, bucketCount: Int)

  /** Walk the instrumented circuit from `topName` and produce one entry per
    * instrumented module instance found via the moduleInfos instance map.
    * Bucket count is read from the post-instrumentation `${mName}_hierCov`
    * memory's depth. Modules without a covmap (no clock / zero addrWidth)
    * are omitted.
    *
    * `circuit` here is the POST-instrumentation circuit — the one returned
    * from `circuit map { ... InstrHierCov ... }`. The covmaps are present
    * in this circuit's modules. `moduleInfos` was built on the original
    * pre-instrumentation circuit but the instance lists are unchanged by
    * instrumentation (we only add statements, not new submodule instances).
    */
  def walkInstances(
    circuit:     Circuit,
    topName:     String,
    moduleInfos: scala.collection.Map[String, HierModuleInfo]
  ): Seq[InstancePath] = {
    val moduleMap = circuit.modules.map(m => m.name -> m).toMap
    val result    = scala.collection.mutable.ListBuffer.empty[InstancePath]

    def findCovMapDepth(mName: String): Int = moduleMap.get(mName) match {
      case Some(m: Module) =>
        val memName = s"${mName}_hierCov"
        def walkStmt(s: Statement): Option[Int] = s match {
          case mem: DefMemory if mem.name == memName => Some(mem.depth.toInt)
          case Block(stmts) =>
            var found: Option[Int] = None
            val it = stmts.iterator
            while (found.isEmpty && it.hasNext) found = walkStmt(it.next())
            found
          case _ => None
        }
        walkStmt(m.body).getOrElse(0)
      case _ => 0
    }

    def descend(moduleName: String, path: String): Unit = {
      val bc = findCovMapDepth(moduleName)
      if (bc > 0) {
        result += InstancePath(path, moduleName, bc)
      }
      moduleInfos.get(moduleName).foreach { mi =>
        for (inst <- mi.insts) {
          val childPath = if (path.isEmpty) inst.name else s"$path.${inst.name}"
          descend(inst.module, childPath)
        }
      }
    }

    descend(topName, "")
    result.toSeq
  }

  private def jsonEscape(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"")

  /** Serialize an instance list to a JSON string (single-line per instance,
    * pretty-printed top-level fields for grepability). */
  def renderJson(topName: String, variant: String, instances: Seq[InstancePath]): String = {
    val sb = new StringBuilder
    sb.append("{\n")
    sb.append(s"""  "topModule": "${jsonEscape(topName)}",${"\n"}""")
    sb.append(s"""  "variant": "${jsonEscape(variant)}",${"\n"}""")
    sb.append(s"""  "totalInstances": ${instances.size},${"\n"}""")
    sb.append(s"""  "totalBuckets": ${instances.map(_.bucketCount.toLong).sum},${"\n"}""")
    sb.append("""  "instances": [""")
    if (instances.nonEmpty) sb.append("\n")
    sb.append(instances.map { inst =>
      val pathQ = jsonEscape(inst.path)
      val nameQ = jsonEscape(inst.moduleName)
      val memQ  = jsonEscape(s"${inst.moduleName}_hierCov")
      s"""    {"path": "$pathQ", "moduleName": "$nameQ", "bucketCount": ${inst.bucketCount}, "memName": "$memQ"}"""
    }.mkString(",\n"))
    if (instances.nonEmpty) sb.append("\n  ")
    sb.append("]\n}\n")
    sb.toString
  }

  /** Write a manifest file at `outPath`. Caller is responsible for path. */
  def writeManifest(outPath: String, topName: String, variant: String, instances: Seq[InstancePath]): Unit = {
    val pw = new PrintWriter(new File(outPath))
    try pw.write(renderJson(topName, variant, instances)) finally pw.close()
  }

  /** Convenience: emit a manifest at the canonical path
    * `${topName}_hier_cov_${variant}_buckets.json` (matching the naming
    * convention used by `writeCoverageSummary` for the txt file) ONLY if
    * the env var `HIERCOV_EMIT_MANIFEST=1` is set.
    *
    * Returns the written path or None. Designed to be a single-line tail
    * call from each variant pass's `execute()` after the rest of the
    * pipeline completes.
    */
  def maybeEmit(
    circuit:     Circuit,
    topName:     String,
    moduleInfos: scala.collection.Map[String, HierModuleInfo],
    variant:     String
  ): Option[String] = {
    if (System.getenv("HIERCOV_EMIT_MANIFEST") != "1") return None
    val instances = walkInstances(circuit, topName, moduleInfos)
    val fileName  = s"${topName}_hier_cov_${variant}_buckets.json"
    val dir       = Option(System.getenv("HIERCOV_EMIT_MANIFEST_DIR")).filter(_.nonEmpty)
    val outPath   = dir match {
      case Some(d) => new File(d, fileName).getPath
      case None    => fileName
    }
    writeManifest(outPath, topName, variant, instances)
    Some(outPath)
  }
}
