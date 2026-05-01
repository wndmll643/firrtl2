// Shared coverage-map + bucket-histogram instrumenter for the hier_cov_*
// variants that use the v9-style design (min(numBits, cap) sizing,
// hardcoded `maxAddrWidth` ceiling, bucket histogram drives `io_hierCovHash`).
//
// Variants supply:
//   - the per-module info (`HierModuleInfo`)
//   - the params (`HierCovParams`)
//   - the *already-selected* input-bit and reg-bit sequences (the selection
//     strategy lives in the variant — that's the v9a/v10a/v6a difference)
//   - a hash function `hashFn` (`HierCovHash.bucketHash` for v6a/v6b,
//     `HierCovHash.directOrFold` for v9a/v9b/v10a, etc.)
//
// The output is a `DefModule` with `io_hierCovSum` (32 b) and `io_hierCovHash`
// (`submodHashSize` b) ports added, plus the coverage memory, bucket
// registers, and the supporting wires.
//
// IMMUTABILITY RULE (project policy, see naive_design_checklist.md):
// EVERY existing variant's emitted Verilog is frozen, including via this
// instrumenter. Any change here that alters Verilog output for ANY caller
// is forbidden — implement it as a parallel function (e.g.
// `InstrHierCovActivityGated`) that NEW variants opt into. Pure
// performance refactors with bit-for-bit identical output (e.g. precomputed
// caches) are acceptable, but only with a clearly-documented
// "Verilog output unchanged" claim and ideally a regression test.
package hier_cov.lib

import firrtl2._
import firrtl2.ir._
import firrtl2.PrimOps._

import scala.collection.mutable.ListBuffer

import HierCovUtil._

/** Function shape: `(bits, outWidth, baseName) → (hashExpr, supportingStmts)`. */
object InstrHierCov {
  type HashFn = (Seq[(Expression, String)], Int, String) => (Expression, Seq[Statement])
}

class InstrHierCov(
  mod:           DefModule,
  mInfo:         HierModuleInfo,
  extModules:    Set[String],
  params:        HierCovParams,
  inputBits:     Seq[(Expression, String)],
  regBits:       Seq[(Expression, String)],
  hashFn:        InstrHierCov.HashFn,
  // Optional extra bits folded into the core hash alongside `regBits` and the
  // built-in submodule-hash bits. Used by v9b/v9d to fold an extmodule
  // input-port proxy into the parent's address (since extmodule internals are
  // invisible). Default empty for variants that don't proxy extmodules.
  extraCoreBits: Seq[(Expression, String)] = Seq.empty
) {
  private val mName = mod.name

  def instrument(): DefModule = mod match {
    case m: Module =>
      val stmts                      = m.body.asInstanceOf[Block].stmts
      val (clockName, hasClockFound) = hasClock(m)

      val submodBits = mInfo.insts.filter(inst => !extModules.contains(inst.module)).flatMap { inst =>
        val ref = WSubField(WRef(inst), "io_hierCovHash")
        (0 until params.submodHashSize).map { idx =>
          val bit = bitExtract(ref, idx, UIntType(IntWidth(params.submodHashSize)))
          (bit, s"${inst.name}.io_hierCovHash[$idx]")
        }
      }.toSeq

      val coreBitsAll = regBits ++ submodBits ++ extraCoreBits

      var dynamicInputHashSize = if (inputBits.nonEmpty)   Math.min(params.maxInputHashSize, inputBits.size)   else 0
      var dynamicCoreHashSize  = if (coreBitsAll.nonEmpty) Math.min(params.maxCoreHashSize,  coreBitsAll.size) else 0

      // Apply the hard maxAddrWidth ceiling.
      if (dynamicInputHashSize + dynamicCoreHashSize > params.maxAddrWidth) {
        dynamicCoreHashSize = params.maxAddrWidth - dynamicInputHashSize
        if (dynamicCoreHashSize < 0) {
          dynamicInputHashSize = params.maxAddrWidth
          dynamicCoreHashSize  = 0
        }
      }

      val (inputHash, inputHashStmts) = hashFn(inputBits,    dynamicInputHashSize, s"${mName}_in")
      val (coreHash,  coreHashStmts)  = hashFn(coreBitsAll, dynamicCoreHashSize,  s"${mName}_core")

      val addrWidth = dynamicInputHashSize + dynamicCoreHashSize
      val addrExpr  = DoPrim(Cat, Seq(inputHash, coreHash), Seq(), UIntType(IntWidth(addrWidth)))

      val covSumPort  = Port(NoInfo, "io_hierCovSum",  Output, UIntType(IntWidth(params.covSumSize)))
      val covHashPort = Port(NoInfo, "io_hierCovHash", Output, UIntType(IntWidth(params.submodHashSize)))

      if (hasClockFound && addrWidth > 0) {
        if (params.bucketCount < 2) throw new Exception("bucketCount must be >= 2")

        // V9-4 in checklist: Math.pow(2, addrWidth).toInt is fragile for
        // addrWidth ≥ 31 — kept for bit-equivalence with v9a; switch to
        // (1 << addrWidth) once we update the legacy variants too.
        val covMapSize = Math.pow(2, addrWidth).toInt
        val (covMap, covRef) = defMemory(s"${mName}_hierCov",
          s"Hierarchical coverage map for ${mName}", covMapSize, addrWidth)

        val covSum = DefRegister(NoInfo, s"${mName}_hierCovSum", UIntType(IntWidth(params.covSumSize)),
          WRef(clockName, ClockType, PortKind, SourceFlow),
          UIntLiteral(0, IntWidth(1)),
          WRef(s"${mName}_hierCovSum", UIntType(IntWidth(params.covSumSize)), RegKind, UnknownFlow))

        val bucketIdxWidth = log2Ceil(params.bucketCount)
        if ((1 << bucketIdxWidth) != params.bucketCount)
          throw new Exception("bucketCount must be a power of 2")
        val bucketRegs: Seq[DefRegister] = (0 until params.bucketCount).map { i =>
          DefRegister(NoInfo, s"${mName}_covBucket_${i}", UIntType(IntWidth(params.bucketWidth)),
            WRef(clockName, ClockType, PortKind, SourceFlow),
            UIntLiteral(0, IntWidth(1)),
            WRef(s"${mName}_covBucket_${i}", UIntType(IntWidth(params.bucketWidth)), RegKind, UnknownFlow))
        }

        val readSubField  = WSubField(covRef, "read")
        val writeSubField = WSubField(covRef, "write")

        val rdAddr = Connect(NoInfo, WSubField(readSubField, "addr", UIntType(IntWidth(addrWidth)), SinkFlow), addrExpr)
        val rdEn   = Connect(NoInfo, WSubField(readSubField, "en",   UIntType(IntWidth(1)), SinkFlow), UIntLiteral(1, IntWidth(1)))
        val rdClk  = Connect(NoInfo, WSubField(readSubField, "clk",  ClockType, SinkFlow),               WRef(clockName, ClockType, PortKind))

        val wrAddr = Connect(NoInfo, WSubField(writeSubField, "addr", UIntType(IntWidth(addrWidth)), SinkFlow), addrExpr)
        val wrMask = Connect(NoInfo, WSubField(writeSubField, "mask", UIntType(IntWidth(1)), SinkFlow), UIntLiteral(1, IntWidth(1)))
        val wrEn   = Connect(NoInfo, WSubField(writeSubField, "en",   UIntType(IntWidth(1)), SinkFlow), UIntLiteral(1, IntWidth(1)))
        val wrClk  = Connect(NoInfo, WSubField(writeSubField, "clk",  ClockType, SinkFlow),               WRef(clockName, ClockType, PortKind))
        val wrData = Connect(NoInfo, WSubField(writeSubField, "data", UIntType(IntWidth(1))),            UIntLiteral(1, IntWidth(1)))

        val readData = WSubField(readSubField, "data", UIntType(IntWidth(1)))
        val newHit   = DoPrim(Not, Seq(readData), Seq(), UIntType(IntWidth(1)))

        val updateSum = Connect(NoInfo, WRef(covSum),
          Mux(readData,
            WRef(covSum),
            DoPrim(Add, Seq(WRef(covSum), UIntLiteral(1, IntWidth(1))), Seq(), UIntType(IntWidth(params.covSumSize)))
          ))

        val bucketIdx = xorFoldAddr(addrExpr, addrWidth, bucketIdxWidth)
        val bucketCons = bucketRegs.zipWithIndex.map { case (b, i) =>
          val idxLit   = UIntLiteral(i, IntWidth(bucketIdxWidth))
          val isBucket = DoPrim(Eq, Seq(bucketIdx, idxLit), Seq(), UIntType(IntWidth(1)))
          val inc      = DoPrim(Add, Seq(WRef(b), UIntLiteral(1, IntWidth(1))), Seq(), UIntType(IntWidth(params.bucketWidth)))
          val nextVal  = Mux(newHit, Mux(isBucket, inc, WRef(b)), WRef(b))
          Connect(NoInfo, WRef(b), nextVal)
        }

        val covSumCon = Connect(NoInfo, WRef(covSumPort), WRef(covSum))

        // Bucket-bit signature → io_hierCovHash for parent aggregation.
        val bucketBits = ListBuffer[(Expression, String)]()
        for ((b, i) <- bucketRegs.zipWithIndex) {
          val width  = params.bucketWidth
          val stride = Math.max(1, width / Math.min(width, 8))
          val bitIdxs = (0 until width by stride)
          for (idx <- bitIdxs) {
            if (bucketBits.size < params.maxBucketSigBits) {
              bucketBits.append((bitExtract(WRef(b), idx, UIntType(IntWidth(width))), s"${mName}_covBucket_${i}[$idx]"))
            }
          }
        }

        val (covHash, covHashStmts) = hashFn(bucketBits.toSeq, params.submodHashSize, s"${mName}_covHash")
        val covHashCon = Connect(NoInfo, WRef(covHashPort), covHash)

        val ports = m.ports ++ Seq(covSumPort, covHashPort)
        val newStmts = stmts ++ inputHashStmts ++ coreHashStmts ++ Seq(covMap, covSum) ++
          bucketRegs ++ Seq(rdAddr, rdEn, rdClk, wrAddr, wrMask, wrEn, wrClk, wrData, updateSum) ++
          bucketCons ++ Seq(covSumCon) ++ covHashStmts :+ covHashCon

        Module(m.info, mName, ports, Block(newStmts))
      } else {
        // No clock or zero-width address: emit zero-driven cov ports for
        // schema compatibility but do nothing else.
        val covSumWire  = DefWire(NoInfo, s"${mName}_hierCovSum_wire",  UIntType(IntWidth(params.covSumSize)))
        val covHashWire = DefWire(NoInfo, s"${mName}_hierCovHash_wire", UIntType(IntWidth(params.submodHashSize)))
        val covSumZero  = Connect(NoInfo, WRef(covSumWire),  UIntLiteral(0, IntWidth(params.covSumSize)))
        val covHashZero = Connect(NoInfo, WRef(covHashWire), UIntLiteral(0, IntWidth(params.submodHashSize)))
        val covSumCon   = Connect(NoInfo, WRef(covSumPort),  WRef(covSumWire))
        val covHashCon  = Connect(NoInfo, WRef(covHashPort), WRef(covHashWire))

        val ports = m.ports ++ Seq(covSumPort, covHashPort)
        val newStmts = stmts ++ Seq(covSumWire, covHashWire, covSumZero, covHashZero, covSumCon, covHashCon)
        Module(m.info, mName, ports, Block(newStmts))
      }

    case ext: ExtModule => ext
    case other          => other
  }
}
