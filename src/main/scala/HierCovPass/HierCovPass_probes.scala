// Probes pass for the hiercov-select pre-run phase.
//
// For every Mux found in every Module, emit a fresh `DefNode` named
// `__hiercov_mux_<stableHash>` whose value is a *copy* of the mux condition
// expression. These nodes carry no functional effect — they exist purely so
// that a Verilator VCD dump exposes every mux-condition value under a stable,
// module-local Verilog name.
//
// The pass also writes a sidecar manifest `<topName>.probes.json` listing
// every emitted probe's (module, stable id, serialized cond expression) so the
// Python analyzer can map VCD signals back to the mux they correspond to.
//
// Registers and ports are *not* probed here: Low FIRRTL already names them
// (`VerilogEmitter.scala:1033` emits DefNode as `wire <name> = …`), and
// Verilator's VCD exposes them at `top.<module>.<signal>` directly.
package hier_cov

import java.io.{File, PrintWriter}

import firrtl2._
import firrtl2.ir._
import firrtl2.Mappers._
import firrtl2.annotations.{Annotation, ModuleTarget, ReferenceTarget}
import firrtl2.transforms.DontTouchAnnotation

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object HierCovProbesUtil {
  def stableHash(s: String): Long = {
    s.foldLeft(0L)((h, c) => (h * 31L + c.toLong) & 0x7fffffffffffffffL)
  }

  /** Collect every Mux that appears anywhere inside this module's statements
    * (including muxes nested inside expressions). Returns the muxes in
    * deterministic order-of-encounter. Duplicates by structural equality are
    * removed so we never emit two probes with the same stable id.
    */
  def findAllMuxes(m: Module): Seq[Mux] = {
    val buf = ListBuffer[Mux]()

    def visitExpr(e: Expression): Unit = {
      e match {
        case mux: Mux =>
          buf.append(mux)
          visitExpr(mux.cond)
          visitExpr(mux.tval)
          visitExpr(mux.fval)
        case other =>
          other.foreachExpr(visitExpr)
      }
    }

    def visitStmt(s: Statement): Unit = {
      s.foreachExpr(visitExpr)
      s.foreachStmt(visitStmt)
    }

    m.body match {
      case b: Block => b.stmts.foreach(visitStmt)
      case other    => visitStmt(other)
    }

    // Dedupe by serialized condition text — two muxes whose conditions
    // serialize identically produce one probe.
    val seen = mutable.Set[String]()
    buf.filter(mx => seen.add(mx.cond.serialize)).toSeq
  }
}

class InstrProbes(mod: DefModule) {
  import HierCovProbesUtil._

  /** Enumerate this module's Input-direction UInt/SInt ports (skipping clock/reset)
    * and all DefRegisters. Used by analyze.py to classify VCD signals as
    * reg-vs-port without guessing from names.
    */
  def moduleSignature(): ModuleSignature = mod match {
    case m: Module =>
      val inputPorts = m.ports.flatMap { p =>
        if (p.direction != Input) None
        else if (p.tpe == ClockType) None
        else {
          val lname = p.name.toLowerCase
          if (lname.contains("clock") || lname.contains("reset")) None
          else p.tpe match {
            case UIntType(IntWidth(w)) => Some(SignalEntry(p.name, w.toInt))
            case SIntType(IntWidth(w)) => Some(SignalEntry(p.name, w.toInt))
            case _                     => None
          }
        }
      }
      val regs = ListBuffer[SignalEntry]()
      def visit(s: Statement): Unit = {
        s match {
          case r: DefRegister =>
            r.tpe match {
              case UIntType(IntWidth(w)) => regs.append(SignalEntry(r.name, w.toInt))
              case SIntType(IntWidth(w)) => regs.append(SignalEntry(r.name, w.toInt))
              case _                     => ()
            }
          case _ => ()
        }
        s.foreachStmt(visit)
      }
      m.body match {
        case b: Block => b.stmts.foreach(visit)
        case other    => visit(other)
      }
      ModuleSignature(m.name, inputPorts.toSeq, regs.toSeq)

    case _ => ModuleSignature(mod.name, Seq.empty, Seq.empty)
  }

  def instrument(): (DefModule, Seq[ProbeEntry]) = mod match {
    case m: Module =>
      val muxes = findAllMuxes(m)
      val entries = ListBuffer[ProbeEntry]()
      val newNodes = ListBuffer[Statement]()

      for (mux <- muxes) {
        val condStr = mux.cond.serialize
        val id = stableHash(m.name + "." + condStr)
        val probeName = f"__hiercov_mux_${id}%016x"
        // Determine width of the probe: same as the mux condition.
        val width = mux.cond.tpe match {
          case UIntType(IntWidth(w)) => w.toInt
          case SIntType(IntWidth(w)) => w.toInt
          case _                     => 1
        }
        // Copy the cond into a new DefNode. Wrap SInt in AsUInt for
        // Verilog-friendly observation — probe semantics don't need signedness.
        val wireExpr: Expression = mux.cond.tpe match {
          case _: SIntType =>
            DoPrim(PrimOps.AsUInt, Seq(mux.cond), Seq(), UIntType(IntWidth(width)))
          case _ => mux.cond
        }
        newNodes.append(DefNode(NoInfo, probeName, wireExpr))
        entries.append(ProbeEntry(m.name, probeName, condStr, width))
      }

      val body = m.body match {
        case b: Block => Block(b.stmts ++ newNodes)
        case other    => Block(Seq(other) ++ newNodes)
      }
      (Module(m.info, m.name, m.ports, body), entries.toSeq)

    case other => (other, Seq.empty)
  }
}

case class ProbeEntry(module: String, probe: String, cond: String, width: Int)
case class SignalEntry(name: String, width: Int)
case class ModuleSignature(module: String, inputPorts: Seq[SignalEntry], regs: Seq[SignalEntry])

class hierCoverageProbes extends Transform {
  def inputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm
  def outputForm: firrtl2.stage.Forms.LowForm.type = firrtl2.stage.Forms.LowForm

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit
    val manifest = ListBuffer[ProbeEntry]()
    val sigs     = ListBuffer[ModuleSignature]()

    val newCircuit = circuit.map { m: DefModule =>
      val instr = new InstrProbes(m)
      sigs.append(instr.moduleSignature())
      val (newMod, entries) = instr.instrument()
      manifest ++= entries
      newMod
    }

    // Mark every emitted probe DefNode as DontTouch so DCE / constant
    // propagation / Verilog emission don't strip these wires (their only
    // purpose is VCD observability; they have no consumers in the circuit).
    val dontTouches: Seq[Annotation] = manifest.toSeq.map { e =>
      DontTouchAnnotation(ModuleTarget(newCircuit.main, e.module).ref(e.probe))
    }

    writeManifest(newCircuit.main, manifest.toSeq, sigs.toSeq)
    state.copy(circuit = newCircuit, annotations = state.annotations ++ dontTouches)
  }

  private def writeManifest(
    topName:   String,
    entries:   Seq[ProbeEntry],
    sigs:      Seq[ModuleSignature]): Unit = {

    val escape = (s: String) => s.replace("\\", "\\\\").replace("\"", "\\\"")
    val body = new StringBuilder
    body.append("{\n")
    body.append(s"""  "version": 2,\n""")
    body.append(s"""  "top": "${escape(topName)}",\n""")

    body.append("""  "probes": [""")
    entries.zipWithIndex.foreach {
      case (e, i) =>
        if (i > 0) body.append(",")
        body.append("\n    {")
        body.append(s""""module": "${escape(e.module)}", """)
        body.append(s""""probe": "${escape(e.probe)}", """)
        body.append(s""""cond": "${escape(e.cond)}", """)
        body.append(s""""width": ${e.width}}""")
    }
    body.append("\n  ],\n")

    body.append("""  "modules": [""")
    sigs.zipWithIndex.foreach {
      case (sig, idx) =>
        if (idx > 0) body.append(",")
        body.append("\n    {")
        body.append(s""""name": "${escape(sig.module)}",""")
        body.append("""
      "input_ports": [""")
        sig.inputPorts.zipWithIndex.foreach {
          case (p, j) =>
            if (j > 0) body.append(",")
            body.append(s"""{"name": "${escape(p.name)}", "width": ${p.width}}""")
        }
        body.append("],")
        body.append("""
      "regs": [""")
        sig.regs.zipWithIndex.foreach {
          case (r, j) =>
            if (j > 0) body.append(",")
            body.append(s"""{"name": "${escape(r.name)}", "width": ${r.width}}""")
        }
        body.append("]}")
    }
    body.append("\n  ]\n}\n")

    val named = new PrintWriter(new File(s"${topName}.probes.json"))
    named.write(body.toString)
    named.close()
  }
}
