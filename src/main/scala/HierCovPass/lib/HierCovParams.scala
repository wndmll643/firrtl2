// Shared knob set for the hierCoverage_* family. Each variant's own params
// case class can extend `HierCovParams` (or just import it as a default
// instance) — common fields stay in one place; variant-specific extensions
// (e.g. v7's `minHashSize`, v7/v9b's extmodule-proxy knobs) live in their own
// classes.
//
// All defaults are taken from v9a, the production default. Other variants
// historically tuned individual fields:
//
//   v6a/v6b: maxCoreHashSize=6, submodHashSize=6 (smaller maps)
//   v7/v9b:  uses dynamic log2 sizing instead of fixed `min(numBits, cap)`
//   v9a:     maxCoreHashSize=14, submodHashSize=16, maxAddrWidth=20
//
// See firrtl2/src/main/scala/HierCovPass/legacy/README.md for the full
// historical parameter table.
package hier_cov

case class HierCovParams(
  // Address-width budget per module. min(numBits, cap)-style sizing; total
  // address width is capped at `maxAddrWidth` (so coverage map size never
  // exceeds 2^maxAddrWidth entries).
  maxInputHashSize: Int = 6,    // v6b-style fixed cap on input-hash bits
  maxCoreHashSize:  Int = 14,   // v9a default; v6a/v6b use 6
  maxAddrWidth:     Int = 20,   // hard ceiling on inputHashSize + coreHashSize
  submodHashSize:   Int = 16,   // width of `io_hierCovHash` exposed to parents

  covSumSize:       Int = 32,   // width of `io_hierCovSum` (cumulative count)

  // Bit-sampling caps (selection layer).
  maxInputPorts:    Int = 8,    // sample at most this many input ports
  maxBitsPerPort:   Int = 8,    // sample at most this many bits per port
  maxRegBits:       Int = 64,   // sample at most this many control-reg bits total
  maxCtrlRegWidth:  Int = 20,   // skip control regs wider than this

  // Bucket histogram (drives `io_hierCovHash` for parents).
  bucketCount:      Int = 16,   // number of bucket counters; must be a power of 2
  bucketWidth:      Int = 8,    // width of each bucket counter
  maxBucketSigBits: Int = 128   // upper bound on bits sampled from buckets
)
