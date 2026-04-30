// Shared, version-agnostic utility functions for the hierCoverage_* passes.
//
// Every variant (v6a, v6b, v9a, v9b, v10a, …) historically duplicated these
// helpers under a per-variant object name (HierCovUtilV6a, HierCovUtilV9a, …).
// This object is the single source of truth — variants should `import
// hier_cov.HierCovUtil._` instead of redeclaring.
//
// Behavioural contract: every function here is a pure builder over FIRRTL IR
// (no side effects, no I/O, no caching). Identical to the legacy V9a copy
// modulo `stableHash64` (added for the probes pass's stable name generation).
//
// Naive-design checklist references resolved by extracting here: DUP-1 (the
// big "every variant copies HierCovUtil*" duplication).
//
// IMMUTABILITY RULE: existing variants' emitted Verilog is frozen forever
// (project policy — see naive_design_checklist.md). Functions in this
// object must therefore be considered ABI-frozen for the variants that
// already call them. To improve a function's output, add a NEW function
// alongside the existing one and have a NEW variant call it.
package hier_cov

import firrtl2._
import firrtl2.ir._
import firrtl2.PrimOps._

object HierCovUtil {

  /** Java-`String.hashCode`-style 31-multiplier hash, masked to 31 bits. Used
    * for deterministic ordering of bit candidates inside a hash word. */
  def stableHash(s: String): Int =
    s.foldLeft(0)((h, c) => (h * 31 + c.toInt) & 0x7fffffff)

  /** 64-bit variant — used by the probes pass to name generated DefNodes
    * (`__hiercov_mux_<16-hex>`). The wider hash makes accidental collisions
    * within a single module negligible. */
  def stableHash64(s: String): Long =
    s.foldLeft(0L)((h, c) => (h * 31L + c.toLong) & 0x7fffffffffffffffL)

  /** Find the canonical clock port name. Returns ("None", false) if absent.
    * Recognises `clock`, `gated_clock`, `clk`. Does not handle async/multi-clock
    * designs — those would need explicit annotations. */
  def hasClock(mod: Module): (String, Boolean) = {
    val clockName = mod.ports
      .find(p => p.name == "clock" || p.name == "gated_clock" || p.name == "clk")
      .map(_.name)
    (clockName.getOrElse("None"), clockName.isDefined)
  }

  /** Width of a UInt or SInt type, or None for clocks/aggregates. */
  def typeWidthOpt(tpe: Type): Option[Int] = tpe match {
    case UIntType(IntWidth(w)) => Some(w.toInt)
    case SIntType(IntWidth(w)) => Some(w.toInt)
    case _                     => None
  }

  /** Extract a single bit from an expression. SInt is wrapped in AsUInt first
    * so the resulting Bits primitive operates on a known-unsigned vector. */
  def bitExtract(expr: Expression, idx: Int, tpe: Type): Expression = {
    val width = typeWidthOpt(tpe).getOrElse(
      throw new Exception(s"Unsupported type width: ${tpe.serialize}")
    )
    val asUInt = tpe match {
      case UIntType(_) => expr
      case SIntType(_) => DoPrim(AsUInt, Seq(expr), Seq(), UIntType(IntWidth(width)))
      case _           => throw new Exception(s"Unsupported type for bitExtract: ${tpe.serialize}")
    }
    DoPrim(Bits, Seq(asUInt), Seq(idx, idx), UIntType(IntWidth(1)))
  }

  /** XOR-reduce of a sequence of 1-bit expressions to a single 1-bit value. */
  def xorReduce(bits: Seq[Expression]): Expression =
    if (bits.isEmpty) UIntLiteral(0, IntWidth(1))
    else bits.reduce((a, b) => DoPrim(Xor, Seq(a, b), Seq(), UIntType(IntWidth(1))))

  /** Concat a sequence of 1-bit expressions in input order. NOTE: width
    * tracking assumes 1-bit inputs (V9-7 in the checklist). For multi-bit
    * inputs callers must compute width separately. */
  def catBits(bits: Seq[Expression]): Expression =
    if (bits.isEmpty) UIntLiteral(0, IntWidth(1))
    else if (bits.length == 1) bits.head
    else {
      var acc      = bits.head
      var accWidth = 1
      for (b <- bits.tail) {
        accWidth = accWidth + 1
        acc = DoPrim(Cat, Seq(acc, b), Seq(), UIntType(IntWidth(accWidth)))
      }
      acc
    }

  /** Ceiling log-2; returns 0 for x ≤ 1. */
  def log2Ceil(x: Int): Int = {
    var v = x - 1
    var r = 0
    while (v > 0) { v = v >> 1; r = r + 1 }
    r
  }

  /** XOR-fold an `addrWidth`-bit expression down to `outWidth` bits by
    * splitting into `outWidth`-wide chunks and XORing them together. */
  def xorFoldAddr(addr: Expression, addrWidth: Int, outWidth: Int): Expression = {
    if (outWidth <= 0) return UIntLiteral(0, IntWidth(1))
    if (addrWidth <= outWidth) return addr
    val chunks = (0 until addrWidth by outWidth).map { lo =>
      val hi = Math.min(addrWidth - 1, lo + outWidth - 1)
      DoPrim(Bits, Seq(addr), Seq(hi, lo), UIntType(IntWidth(hi - lo + 1)))
    }
    chunks.reduce((a, b) => DoPrim(Xor, Seq(a, b), Seq(), UIntType(IntWidth(outWidth))))
  }

  /** Build a 1-bit-wide single-port-equivalent memory used as the per-module
    * coverage bitmap. Read latency 0 / write latency 1 — a read returns the
    * pre-write value at the same cycle, which is exactly the "have I been
    * here?" semantics the coverage map relies on. */
  def defMemory(name: String, info: String, size: Int, addrWidth: Int): (DefMemory, WRef) = {
    val mem = DefMemory(
      FileInfo(StringLit(info)),
      name,
      UIntType(IntWidth(1)),
      size,
      1,
      0,
      Seq("read"),
      Seq("write"),
      Seq()
    )
    val ref = WRef(
      name,
      BundleType(Seq(
        Field("read", Flip, BundleType(List(
          Field("addr", Default, UIntType(IntWidth(addrWidth))),
          Field("en",   Default, UIntType(IntWidth(1))),
          Field("clk",  Default, ClockType),
          Field("data", Flip,    UIntType(IntWidth(1)))
        ))),
        Field("write", Flip, BundleType(List(
          Field("addr", Default, UIntType(IntWidth(addrWidth))),
          Field("mask", Default, UIntType(IntWidth(1))),
          Field("en",   Default, UIntType(IntWidth(1))),
          Field("clk",  Default, ClockType),
          Field("data", Default, UIntType(IntWidth(1)))
        )))
      )),
      MemKind,
      SourceFlow
    )
    (mem, ref)
  }
}
