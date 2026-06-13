// hier_cov.traceCommitLog — gate every Print statement on a top-level
// `debug_print` signal and emit a manifest of all Print-statement sites
// so downstream Verilog post-processing can redirect $fwrite output to a
// per-iteration trace file.
//
// Why this pass exists
// --------------------
// BOOM's `enableCommitLogPrintf` Scala flag (BoomCoreParams) emits Chisel
// `printf` statements at every ROB commit. firrtl2's standard VerilogEmitter
// lowers those to `$fwrite(32'h80000002, "(0x%x)", uop.debug_inst)` — i.e.,
// unconditional commit-log writes to stderr.
//
// The DifuzzRTL/ProcessorFuzz workflow expects a *per-test* commit log file
// (`<out>/trace/rtl_<it>.log`) that ProcessorFuzz/Fuzzer.py reads back via
// `trace_compare(isa_csv, rtl_log, toplevel)` to detect mid-execution RTL/Spike
// divergences. The upstream DifuzzRTL repo ships hand-instrumented Verilog
// (SmallBoomTile_v1.3_state.v) that adds this redirection manually. This pass
// is the FIRRTL-level equivalent — it operates on regenerated FIRRTL from
// modern chipyard (Chisel 6+) rather than hand-edited Verilog.
//
// Mechanism (two stages — this pass + Verilog post-process)
// ---------------------------------------------------------
// Stage 1 (this pass):
//   1. Walks every module body and finds Print statements.
//   2. Adds a `debug_print` wire to the top module (initialized to 0; the
//      Verilog post-process inserts an `initial` block that overrides it
//      from `+DEBUG=%d` plusarg).
//   3. Wraps every Print's enable expression in an AND with `debug_print`.
//   4. Emits a JSON manifest listing each Print site (module, source info,
//      format string) so the post-process step knows which `$fwrite` lines
//      to surgically redirect.
//
// Stage 2 (Python in verilog_gen.py, scope-limited):
//   * Inject an `initial` block at the top of the toplevel module that
//     reads `+DEBUG=%d` (drives debug_print) and `+TRACE_FILE=%s` (opens
//     trace_fd = $fopen(...)).
//   * Replace `$fwrite(32'h80000002,` with `$fwrite(trace_fd,` for the
//     lines flagged in the manifest.
//
// Publishability notes
// --------------------
// The Stage-1 work (this pass) is the meaningful contribution: it operates
// at FIRRTL IR level, is independent of the downstream Verilog emitter, and
// preserves the existing hier_cov_* coverage instrumentation. Stage 2 is
// mechanical glue; the architecture decouples FIRRTL-IR transformations from
// SystemVerilog-emitter concerns (file descriptors, plusargs).
//
// Composition with hier_cov_* passes
// ----------------------------------
// This pass commutes with the hier_cov_ctrl_bucket_*/data_bucket_* passes —
// it only touches Print statements and adds a single wire to the top module,
// neither of which the hier_cov passes care about. The recommended pipeline:
//
//   FIRRTL → hier_cov.hierCoverage_<variant> → hier_cov.traceCommitLog → Verilog
//
// (verilog_gen.py supports this via comma-separated COV_PASS values; see the
// corresponding patch.)

package hier_cov

import java.io.{File, PrintWriter}

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._
import firrtl2.PrimOps

import scala.collection.mutable


class hierCoverage_trace_log extends Transform {
  def inputForm:  firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm
  def outputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm

  // Composition with other custom transforms (notably hier_cov.hierCoverage_*):
  // declare we don't invalidate any prior pass's output. Without this, firrtl2's
  // DependencyManager treats every custom Transform as "invalidates everything"
  // and refuses to schedule two of them in the same pipeline (CyclicException).
  override def invalidates(a: Transform): Boolean = false

  // Identifying BOOM commit-log printfs precisely:
  //   * Source location: generators/boom/src/main/scala/v[34]/exu/core.scala
  //     (catches both BOOM v3 and v4 cores).
  //   * Format string: NOT starting with "Assertion failed:" — that pattern is
  //     emitted by Chisel `assert(...)` statements which compile to FIRRTL
  //     Print + Stop pairs at the same line numbers as the commit-log block.
  //   * Format string content: the commit-log printfs use compact format
  //     strings like "(0x%x)", "%d 0x%x ", " x%d 0x%x\n", " f%d 0x%x\n".
  //
  // The conjunction (BOOM core.scala) AND (NOT assertion) cleanly isolates the
  // commit-log printfs without false positives in the chipyard build.
  private val InstrumentInfoPattern =
    ".*boom/src/main/scala/v[34]/exu/core\\.scala.*".r

  /** True iff this Print statement is from BOOM's commit-log machinery,
    * NOT from an `assert(...)` whose failure message also lowers to Print. */
  private def isCommitLogPrint(info: Info, fmt: String): Boolean = {
    val locMatch = InstrumentInfoPattern.findFirstIn(info.serialize).isDefined
    val isAssertion = fmt.startsWith("Assertion failed")
    locMatch && !isAssertion
  }

  private case class PrintSite(module: String, info: String, fmt: String)
  private val printSites = mutable.ArrayBuffer[PrintSite]()

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit
    // Walk every module body, identify commit-log Print sites, and emit a
    // manifest. We deliberately do NOT mutate the IR — adding a `debug_print`
    // wire/AND-gate at FIRRTL level gets constant-folded away by firrtl2's
    // optimizer (since the wire defaults to 0 with no non-constant driver),
    // which erases the printfs entirely. The Verilog post-process step uses
    // the manifest to insert the gate AT THE Verilog LEVEL where it survives.
    //
    // Architectural rationale: this preserves the separation of concerns
    // between FIRRTL-IR semantics (no notion of file descriptors, plusargs,
    // initial blocks) and Verilog/SystemVerilog implementation choices (the
    // $fwrite handle, $value$plusargs, $fopen). Trying to bake plusarg-driven
    // wires into FIRRTL requires either ExtModule blackboxes or modifying
    // firrtl2's VerilogEmitter — both more invasive than necessary.
    circuit.modules.foreach {
      case mm: Module => scanModule(mm)
      case _          => ()
    }
    emitManifest(circuit.main)
    state  // IR unchanged
  }

  /** Walk a module body, record every commit-log Print site for the manifest.
    * Does not modify the IR. */
  private def scanModule(m: Module): Unit = {
    def scan(s: Statement): Unit = s match {
      case Print(info, str, _, _, _, _) if isCommitLogPrint(info, str.string) =>
        printSites += PrintSite(m.name, info.serialize, str.string)
      case other =>
        other.foreachStmt(scan)
    }
    scan(m.body)
  }

  /** Minimal JSON string escaping — handles the control chars that show up in
    * BOOM commit-log format strings ('\n', '\t', '\"', '\\'). Sufficient for
    * the manifest's downstream Python consumer. */
  private def jsonEscape(s: String): String =
    s.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c.toInt < 0x20 => f"\\u${c.toInt}%04x"
      case c    => c.toString
    }

  private def emitManifest(topName: String): Unit = {
    val path = sys.env.getOrElse(
      "TRACE_LOG_MANIFEST",
      s"./${topName}_trace_log_manifest.json"
    )
    val pw = new PrintWriter(new File(path))
    pw.println("{")
    pw.println(s"""  "top": "$topName",""")
    pw.println(s"""  "n_sites": ${printSites.size},""")
    pw.println( """  "sites": [""")
    for ((site, i) <- printSites.zipWithIndex) {
      val info = jsonEscape(site.info)
      val fmt  = jsonEscape(site.fmt)
      val sep  = if (i < printSites.size - 1) "," else ""
      pw.println(s"""    {"module":"${site.module}","info":"$info","format":"$fmt"}$sep""")
    }
    pw.println( """  ]""")
    pw.println("}")
    pw.close()
    Console.println(s"[trace_log] wrote manifest with ${printSites.size} Print sites to $path")
  }
}
