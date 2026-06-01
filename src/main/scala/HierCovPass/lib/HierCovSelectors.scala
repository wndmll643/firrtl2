// Bit-selection strategies for hierCoverage_* variants.
//
// The selection layer answers: "given module ports + control regs, which bits
// should participate in the per-module hash?" Variants differ in this step
// (data inputs vs control inputs vs annotation-driven), and that difference
// is the only behavioural axis distinguishing v6a / v6b / v9a / v10a.
//
// Each selector returns `Seq[(Expression, String)]` — (bit-extracting IR
// expression, human-readable name for deterministic ordering downstream).
//
// Resolved checklist items: DUP-1 (duplication only).
//
// IMMUTABILITY RULE (project policy, see naive_design_checklist.md):
// every existing variant's emitted Verilog is frozen. Functions in this
// file must therefore preserve bit-for-bit behaviour for the variants
// that already call them. Any improvement — V9-9 deterministic ordering,
// activity-aware filtering, smarter bit selection, etc. — is added as a
// NEW function in this file (e.g. `selectControlRegBitsSorted`) and
// consumed by a NEW variant (`v11a`, …). Never mutate an existing
// function's behaviour just because it could be cleaner.
package hier_cov.lib

import firrtl2._
import firrtl2.ir._

import scala.collection.mutable.ListBuffer

import HierCovUtil._

object HierCovSelectors {

  /** Generic per-port bit decimation: take `min(maxBitsPerPort, width)` bits
    * at a stride of `width / bitsToTake`. */
  private def sampleBitsOfPort(p: Port, params: HierCovParams): Seq[(Expression, String)] = {
    typeWidthOpt(p.tpe).toSeq.flatMap { width =>
      val bitsToTake = Math.min(params.maxBitsPerPort, width)
      val stride     = Math.max(1, width / bitsToTake)
      val bitIdxs    = (0 until width by stride).take(bitsToTake)
      bitIdxs.map { idx =>
        val bit = bitExtract(WRef(p), idx, p.tpe)
        (bit, s"${p.name}[$idx]")
      }
    }
  }

  /** Sample bits from CONTROL input ports — those flagged by graphLedger
    * as feeding mux conditions. v6b / v9a / v9b style. */
  def selectControlInputBits(
    ports:         Seq[Port],
    ctrlPortNames: Set[String],
    params:        HierCovParams
  ): Seq[(Expression, String)] = {
    val ctrlInputs = ports
      .filter(p => p.direction == Input)
      .filterNot { p =>
        val lname = p.name.toLowerCase
        p.tpe == ClockType || lname.contains("clock") || lname.contains("reset")
      }
      .filter(p => p.tpe.isInstanceOf[UIntType] || p.tpe.isInstanceOf[SIntType])
      .filter(p => ctrlPortNames.contains(p.name))
    ctrlInputs.take(params.maxInputPorts).flatMap(p => sampleBitsOfPort(p, params))
  }

  /** Sample bits from DATA input ports — non-control inputs, used by v6a /
    * v7 (DifuzzRTL-style). */
  def selectDataInputBits(
    ports:         Seq[Port],
    ctrlPortNames: Set[String],
    params:        HierCovParams
  ): Seq[(Expression, String)] = {
    val dataInputs = ports
      .filter(p => p.direction == Input)
      .filterNot { p =>
        val lname = p.name.toLowerCase
        p.tpe == ClockType || lname.contains("clock") || lname.contains("reset")
      }
      .filter(p => p.tpe.isInstanceOf[UIntType] || p.tpe.isInstanceOf[SIntType])
      .filterNot(p => ctrlPortNames.contains(p.name))
    dataInputs.take(params.maxInputPorts).flatMap(p => sampleBitsOfPort(p, params))
  }

  /** Sample bits from CONTROL registers — those flagged by graphLedger as
    * feeding muxes. Excludes wide regs and direct-input-fed regs as the
    * historical heuristic does (V9-10: the `> 3` width threshold for
    * dirInRegs is a magic number kept for behavioural equivalence).
    *
    * NOTE: this function intentionally does NOT sort `ctrlRegs` before the
    * `maxRegBits` truncation — the JVM-iteration-order of the input Set is
    * preserved to keep v6a/v6b/v9a/v9b bit-for-bit equivalent to their
    * pre-refactor (baseline) behaviour. V9-9 in the checklist is therefore
    * intentionally OPEN here; any deterministic-ordering fix must land as a
    * new variant (e.g. v11a) so the change can be ablated cleanly against
    * the frozen baselines. */
  def selectControlRegBits(
    ctrlRegs:  scala.collection.Set[DefRegister],
    dirInRegs: scala.collection.Set[DefRegister],
    params:    HierCovParams
  ): Seq[(Expression, String)] = {
    val filteredDirIn = dirInRegs.filter(r => typeWidthOpt(r.tpe).getOrElse(0) > 3)

    val regs = ctrlRegs
      .filterNot(filteredDirIn.contains(_))
      .filter(r => typeWidthOpt(r.tpe).getOrElse(0) < params.maxCtrlRegWidth)
      .toSeq    // intentionally unsorted — see header comment

    val bits = ListBuffer[(Expression, String)]()
    for (reg <- regs if bits.size < params.maxRegBits) {
      typeWidthOpt(reg.tpe).foreach { width =>
        val stride  = Math.max(1, width / Math.min(width, 8))
        val bitIdxs = (0 until width by stride)
        for (idx <- bitIdxs if bits.size < params.maxRegBits) {
          bits.append((bitExtract(WRef(reg), idx, reg.tpe), s"${reg.name}[$idx]"))
        }
      }
    }
    bits.toSeq
  }

  /** ExtModule input-port proxy bits — used by v9b / v9d.
    *
    * For each instance of an ExtModule child, sample bits from the child's
    * input ports (clock/reset/metaReset names skipped). These act as a
    * stand-in for "what happened inside the extmodule" when its internals
    * are invisible to instrumentation.
    *
    * Returns a `Seq[(Expression, String)]` ready to be folded into a
    * variant's `coreBitsAll` via `InstrHierCov`'s `extraCoreBits` parameter. */
  def selectExtModuleProxyBits(
    insts:           scala.collection.Set[WDefInstance],
    extModules:      Set[String],
    extModulePorts:  scala.collection.Map[String, Seq[Port]],
    maxPorts:        Int,
    maxBitsPerPort:  Int
  ): Seq[(Expression, String)] = {
    insts.filter(inst => extModules.contains(inst.module)).flatMap { inst =>
      extModulePorts.getOrElse(inst.module, Seq.empty)
        .filter(p => p.direction == Input)
        .filter(p => p.tpe.isInstanceOf[UIntType] || p.tpe.isInstanceOf[SIntType])
        .filterNot { p =>
          val lname = p.name.toLowerCase
          lname.contains("clock") || lname.contains("reset") || lname.contains("metareset")
        }
        .take(maxPorts)
        .flatMap { p =>
          typeWidthOpt(p.tpe).toSeq.flatMap { width =>
            val bitsToTake = Math.min(maxBitsPerPort, width)
            val stride     = Math.max(1, width / bitsToTake)
            (0 until width by stride).take(bitsToTake).map { idx =>
              val bit = bitExtract(WSubField(WRef(inst), p.name), idx, p.tpe)
              (bit, s"${inst.name}.${p.name}[$idx]")
            }
          }
        }
    }.toSeq
  }

  /** Build the per-extmodule-name port map that v9b / v9d need to drive
    * `selectExtModuleProxyBits`. Iterates the circuit's ExtModules once. */
  def collectExtModulePorts(circuit: Circuit): scala.collection.Map[String, Seq[Port]] = {
    val out = scala.collection.mutable.Map[String, Seq[Port]]()
    for (m <- circuit.modules) m match {
      case ext: ExtModule => out(ext.name) = ext.ports
      case _              =>
    }
    out
  }

  /** Annotation-driven selection: caller supplies the kept (signal → bit
    * indices) maps; we look the named ports/regs up in the supplied
    * collections and emit `bitExtract`s for each kept index.
    *
    * Out-of-range bit indices are silently dropped — V10-3 fix: validate
    * caller-side at annotation load time so this remains robust. */
  def selectFromAnnotation(
    keptPorts:     Map[String, Seq[Int]],
    keptRegs:      Map[String, Seq[Int]],
    modulePorts:   Seq[Port],
    moduleRegs:    scala.collection.Set[DefRegister]
  ): (Seq[(Expression, String)], Seq[(Expression, String)]) = {

    val portByName = modulePorts
      .filter(p => p.direction == Input)
      .filter(p => p.tpe.isInstanceOf[UIntType] || p.tpe.isInstanceOf[SIntType])
      .map(p => p.name -> p)
      .toMap

    // Iteration order over `keptPorts` / `keptRegs` is the Map's native order
    // (NOT name-sorted) — preserves bit-for-bit equivalence with the
    // pre-refactor v10a, per the baseline-freeze policy. This is invisible
    // at the Verilog layer anyway (downstream `directOrFold` sorts inputs by
    // stableHash(name) internally), but kept literal-faithful for clarity.
    val portBits = ListBuffer[(Expression, String)]()
    for ((pname, bitIdxs) <- keptPorts; p <- portByName.get(pname)) {
      typeWidthOpt(p.tpe).foreach { width =>
        for (idx <- bitIdxs if idx >= 0 && idx < width) {
          portBits.append((bitExtract(WRef(p), idx, p.tpe), s"${p.name}[$idx]"))
        }
      }
    }

    val regByName = moduleRegs.map(r => r.name -> r).toMap
    val regBits   = ListBuffer[(Expression, String)]()
    for ((rname, bitIdxs) <- keptRegs; r <- regByName.get(rname)) {
      typeWidthOpt(r.tpe).foreach { width =>
        for (idx <- bitIdxs if idx >= 0 && idx < width) {
          regBits.append((bitExtract(WRef(r), idx, r.tpe), s"${r.name}[$idx]"))
        }
      }
    }

    (portBits.toSeq, regBits.toSeq)
  }
}
