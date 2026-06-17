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

  /** Per-module signal lists that feed the hash address for each module's
    * `_hierCov` covmap. The variant pass supplies a `signalLookup` callback
    * (module name → (inputBitNames, regBitNames)) when calling
    * `maybeEmitWithSignals`. This is purely informational metadata — no IR
    * is modified. */
  case class ModuleSignals(
    moduleName:    String,
    inputBitNames: Seq[String],
    regBitNames:   Seq[String]
  )

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

  // ---------------------------------------------------------------------
  // Per-bucket-index manifest (signal-attribution side file).
  //
  // The instance-level manifest above lists each `_hierCov` memory's path +
  // bucketCount. The fuzzer concatenates these memories into a flat bitmap
  // in the order they appear in the manifest. For bug-localization we want
  // the inverse mapping: given a set bit at global offset N, which BOOM
  // module did the bit come from, and what source signals fed its hash?
  //
  // Implementation: walk the instance list a second time, expanding each
  // entry into `bucketCount` consecutive entries keyed by global bucket
  // index. Within an instance, the relative address is `idx - bit_offset`
  // and the source signal list is identical for every bucket of that
  // instance (the addr is a hash of the same bit set). This expansion is
  // O(totalBuckets) bytes — about 1-2 MB of JSON for MediumBoomV3 — but
  // is emitted only on demand (env var) and never read by the simulator
  // hot loop; the fuzzer's offline triage script consumes it.
  //
  // IMMUTABILITY: emission is gated on `HIERCOV_EMIT_MANIFEST=1` (same as
  // the instance-level manifest). The function does NOT modify the FIRRTL
  // IR; the only side-effect is the JSON file. Bucket numbering / hash
  // function / memory layout are untouched — the manifest describes the
  // EXISTING layout produced by InstrHierCov, so it can be retroactively
  // applied to .cov.gz files produced before this change was added.
  // ---------------------------------------------------------------------

  /** Emit the per-bucket-index expansion file. One JSON object per global
    * bucket, keyed by stringified index (matches the format the user-side
    * triage scripts expect):
    *
    * {
    *   "0":   {"module": "TestHarness", "path": "", "addr": 0,    "input_signals": [...], "reg_signals": [...]},
    *   "1":   {"module": "TestHarness", "path": "", "addr": 1,    ...},
    *   ...
    *   "64":  {"module": "UARTAdapter", "path": "uart_sim_0_uartno0", "addr": 0, ...},
    *   ...
    * }
    *
    * `path` is the instance dotted path (matches the parent manifest's
    * `path` field; empty string = top module). `addr` is the per-module
    * memory address (0 .. bucketCount-1). Source signal lists describe the
    * bits hashed to form `addr` — every bucket in the same instance shares
    * the same lists. */
  def renderPerBucketJson(
    instances:   Seq[InstancePath],
    signalsByModule: scala.collection.Map[String, ModuleSignals]
  ): String = {
    val sb = new StringBuilder
    sb.append("{\n")
    var bitOffset: Long = 0L
    var firstEntry = true
    for (inst <- instances) {
      val sigs = signalsByModule.getOrElse(inst.moduleName,
        ModuleSignals(inst.moduleName, Seq.empty, Seq.empty))
      val inputJson = sigs.inputBitNames.map(n => s""""${jsonEscape(n)}"""").mkString(",")
      val regJson   = sigs.regBitNames.map(n => s""""${jsonEscape(n)}"""").mkString(",")
      val pathQ     = jsonEscape(inst.path)
      val modQ      = jsonEscape(inst.moduleName)
      var i = 0
      while (i < inst.bucketCount) {
        if (!firstEntry) sb.append(",\n")
        firstEntry = false
        val globalIdx = bitOffset + i
        sb.append(s"""  "${globalIdx}": {"module": "$modQ", "path": "$pathQ", """)
        sb.append(s""""addr": $i, """)
        sb.append(s""""input_signals": [$inputJson], "reg_signals": [$regJson]}""")
        i += 1
      }
      bitOffset += inst.bucketCount
    }
    if (!firstEntry) sb.append("\n")
    sb.append("}\n")
    sb.toString
  }

  /** Same as `maybeEmit` but also writes a per-bucket-index expansion file
    * named `${topName}_hier_cov_${variant}_per_bucket.json` AND embeds the
    * per-module signal lists into the main manifest's `instances` array
    * (extra fields `"inputSignals"` / `"regSignals"`).
    *
    * `signalLookup`: module name → (inputBitNames, regBitNames). Variant
    * passes pass a closure that re-runs the selector on the original module
    * info. Returning `(Seq.empty, Seq.empty)` for a module is OK — the
    * manifest will record empty lists for that instance.
    *
    * Returns (mainManifestPath, perBucketManifestPath) or None. */
  def maybeEmitWithSignals(
    circuit:      Circuit,
    topName:      String,
    moduleInfos:  scala.collection.Map[String, HierModuleInfo],
    variant:      String,
    signalLookup: String => (Seq[String], Seq[String])
  ): Option[(String, String)] = {
    if (System.getenv("HIERCOV_EMIT_MANIFEST") != "1") return None
    val instances = walkInstances(circuit, topName, moduleInfos)
    val dir       = Option(System.getenv("HIERCOV_EMIT_MANIFEST_DIR")).filter(_.nonEmpty)
    def outPath(name: String): String = dir match {
      case Some(d) => new File(d, name).getPath
      case None    => name
    }

    // Compute signals per UNIQUE module (cache — many instances share a
    // module type). Names are derived from selector output so they remain
    // identical across re-runs as long as the input FIRRTL is identical.
    val signalsByModule = scala.collection.mutable.Map[String, ModuleSignals]()
    for (inst <- instances) {
      if (!signalsByModule.contains(inst.moduleName)) {
        val (inSigs, regSigs) = signalLookup(inst.moduleName)
        signalsByModule(inst.moduleName) = ModuleSignals(inst.moduleName, inSigs, regSigs)
      }
    }

    // Main manifest WITH the new optional signal fields (additive — old
    // consumers that ignore unknown keys still work). The original
    // renderJson() is unchanged; we render an extended variant inline.
    val mainName   = s"${topName}_hier_cov_${variant}_buckets.json"
    val perBktName = s"${topName}_hier_cov_${variant}_per_bucket.json"
    val mainPath   = outPath(mainName)
    val perBktPath = outPath(perBktName)

    writeManifestExtended(mainPath, topName, variant, instances, signalsByModule)

    val pw = new PrintWriter(new File(perBktPath))
    try pw.write(renderPerBucketJson(instances, signalsByModule)) finally pw.close()

    Some((mainPath, perBktPath))
  }

  /** Extended main manifest: same shape as `renderJson` but each instance
    * entry gains `"inputSignals"` and `"regSignals"` arrays (the source bit
    * names that feed the hash for that module). Old consumers parsing the
    * file ignore unknown keys; new consumers can use the signal lists for
    * bug-localization without loading the much larger per-bucket file. */
  def writeManifestExtended(
    outPath:         String,
    topName:         String,
    variant:         String,
    instances:       Seq[InstancePath],
    signalsByModule: scala.collection.Map[String, ModuleSignals]
  ): Unit = {
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
      val sigs  = signalsByModule.getOrElse(inst.moduleName,
        ModuleSignals(inst.moduleName, Seq.empty, Seq.empty))
      val inputJson = sigs.inputBitNames.map(n => s""""${jsonEscape(n)}"""").mkString(",")
      val regJson   = sigs.regBitNames.map(n => s""""${jsonEscape(n)}"""").mkString(",")
      s"""    {"path": "$pathQ", "moduleName": "$nameQ", "bucketCount": ${inst.bucketCount}, "memName": "$memQ", "inputSignals": [$inputJson], "regSignals": [$regJson]}"""
    }.mkString(",\n"))
    if (instances.nonEmpty) sb.append("\n  ")
    sb.append("]\n}\n")
    val pw = new PrintWriter(new File(outPath))
    try pw.write(sb.toString) finally pw.close()
  }
}
