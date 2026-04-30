// Hash strategies used by hierCoverage_* variants to map a sequence of
// (sampled bit, name) pairs to a fixed-width hash word for the coverage-map
// address.
//
// Three strategies, each chosen by a different historical variant family:
//
//   bucketHash       — XOR-reduce inside name-hashed buckets. v6a/v6b style:
//                      coarse, lossy, deterministic. Each output bit is the
//                      XOR of the inputs that hashed to it.
//   directOrFold     — Direct concatenation when bits fit in outWidth (zero
//                      loss); XOR-fold fallback otherwise. v9a/v9b style.
//                      Empirically best TTB on micro_boom_720.
//   xorFold          — Always XOR-fold. v9c/v9d ablation. Strictly weaker than
//                      directOrFold when bits.size ≤ outWidth.
//
// All three return (hashExpr, supportingStatements). The supporting statements
// are wires/connects that the caller must include in the module body.
package hier_cov

import firrtl2.ir._
import HierCovUtil._

object HierCovHash {

  /** Bucket-XOR-reduce hash. Each input bit hashes by its (string) name into
    * one of `width` output buckets; the output bit is the XOR-reduce of all
    * inputs in that bucket.
    *
    * Pros: stable across runs (deterministic name hashing); fast.
    * Cons: ~50 % collision per bucket — high information loss.
    */
  def bucketHash(
    bits:     Seq[(Expression, String)],
    width:    Int,
    baseName: String
  ): (Expression, Seq[Statement]) = {
    if (width <= 0) return (UIntLiteral(0, IntWidth(1)), Seq.empty)

    val buckets = Array.fill[scala.collection.mutable.ListBuffer[Expression]](width)(
      scala.collection.mutable.ListBuffer[Expression]()
    )
    for ((expr, name) <- bits) {
      val idx = stableHash(name) % width
      buckets(idx).append(expr)
    }

    val bitWires = (0 until width).map(i =>
      DefWire(NoInfo, s"${baseName}_b${i}", UIntType(IntWidth(1)))
    )
    val bitCons = bitWires.zipWithIndex.map { case (w, i) =>
      val xorExpr = xorReduce(buckets(i).toSeq)
      Connect(NoInfo, WRef(w), xorExpr)
    }

    val hashWire = DefWire(NoInfo, s"${baseName}_hash", UIntType(IntWidth(width)))
    val catExpr  = catBits(bitWires.reverse.map(w => WRef(w)))
    val hashCon  = Connect(NoInfo, WRef(hashWire), catExpr)

    (WRef(hashWire), bitWires ++ bitCons :+ hashWire :+ hashCon)
  }

  /** Direct-or-fold hash. When bits.size ≤ outWidth, concatenates them with
    * zero padding (no information loss). Otherwise XOR-folds the wide
    * concatenation down to outWidth.
    *
    * Inputs are sorted by `stableHash(name)` for deterministic ordering — so
    * the hash word's bit positions correspond to sorted (not source-order)
    * inputs. Tying output position to input identity matters for cross-run
    * reproducibility.
    */
  def directOrFold(
    bits:     Seq[(Expression, String)],
    outWidth: Int,
    baseName: String
  ): (Expression, Seq[Statement]) = {
    if (outWidth <= 0 || bits.isEmpty)
      return (UIntLiteral(0, IntWidth(Math.max(1, outWidth))), Seq.empty)

    val sortedBits = bits.sortBy { case (_, name) => stableHash(name) }
    val numBits    = sortedBits.size

    if (numBits <= outWidth) {
      val allBitExprs = sortedBits.map(_._1)
      val padded      = allBitExprs ++ Seq.fill(outWidth - numBits)(UIntLiteral(0, IntWidth(1)))
      val concatWire  = DefWire(NoInfo, s"${baseName}_hash", UIntType(IntWidth(outWidth)))
      val catExpr     = catBits(padded.reverse)
      val concatCon   = Connect(NoInfo, WRef(concatWire), catExpr)
      (WRef(concatWire), Seq(concatWire, concatCon))
    } else {
      val allBitExprs = sortedBits.map(_._1)
      val wideWire    = DefWire(NoInfo, s"${baseName}_wide", UIntType(IntWidth(numBits)))
      val wideCat     = catBits(allBitExprs.reverse)
      val wideCon     = Connect(NoInfo, WRef(wideWire), wideCat)
      val foldedExpr  = xorFoldAddr(WRef(wideWire), numBits, outWidth)
      val hashWire    = DefWire(NoInfo, s"${baseName}_hash", UIntType(IntWidth(outWidth)))
      val hashCon     = Connect(NoInfo, WRef(hashWire), foldedExpr)
      (WRef(hashWire), Seq(wideWire, wideCon, hashWire, hashCon))
    }
  }

  /** XOR-fold-only hash. Always builds the wide concatenation and folds. Used
    * for v9c/v9d ablations to isolate the contribution of `directOrFold`'s
    * direct path. Strictly weaker than `directOrFold` for narrow inputs. */
  def xorFold(
    bits:     Seq[(Expression, String)],
    outWidth: Int,
    baseName: String
  ): (Expression, Seq[Statement]) = {
    if (outWidth <= 0 || bits.isEmpty)
      return (UIntLiteral(0, IntWidth(Math.max(1, outWidth))), Seq.empty)

    val sortedBits  = bits.sortBy { case (_, name) => stableHash(name) }
    val numBits     = sortedBits.size
    val allBitExprs = sortedBits.map(_._1)
    val wideWire    = DefWire(NoInfo, s"${baseName}_wide", UIntType(IntWidth(numBits)))
    val wideCat     = catBits(allBitExprs.reverse)
    val wideCon     = Connect(NoInfo, WRef(wideWire), wideCat)
    val foldedExpr  = xorFoldAddr(WRef(wideWire), numBits, outWidth)
    val hashWire    = DefWire(NoInfo, s"${baseName}_hash", UIntType(IntWidth(outWidth)))
    val hashCon     = Connect(NoInfo, WRef(hashWire), foldedExpr)
    (WRef(hashWire), Seq(wideWire, wideCon, hashWire, hashCon))
  }
}
