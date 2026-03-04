# HierCovPass

Hierarchical coverage transform for FIRRTL. This pass adds:
- Hierarchical coverage collection with a 2-D bitmap (flattened address = input_hash ++ core_hash).
- Submodule abstraction via hashing of each submodule's coverage summary.
- metaAssert aggregation and metaReset wiring (same semantics as the coverage pass).

## Files and versions
- HierCovPass_v1.scala
  - v1: initial hierarchical coverage pass (input hashing, reg+submodule hashing, 2-D bitmap, metaAssert/metaReset).
- HierCovPass_v2.scala
  - v2: replaces submodule hash with multi-bucket signature; introduces bucket counters and hashes them for `io_hierCovHash`.
- HierCovPass_v3.scala
  - v3: v1 semantics with guards for unknown-width ports/regs (skips sampling when width is not concrete).
- HierCovPass_v4.scala
  - v4: v2 semantics with guards for unknown-width ports/regs (skips sampling when width is not concrete).

## How it works
Each module computes:
1) `input_hash`: hash of selected input bits (data inputs only, limited by params).
2) `core_hash`: hash of internal register bits plus submodule coverage hashes.
3) Coverage bitmap indexed by `{input_hash, core_hash}` (flattened into one address).
4) `io_hierCovSum` (coverage hit counter) and `io_hierCovHash` (hash of bucketed coverage signature).

Submodules export `io_hierCovHash`, which is folded into the parent `core_hash`.

### Input selection rule
- Inputs are `Input` ports with UInt/SInt types.
- Ports with `clock` or `reset` in the name (or ClockType) are excluded.
- Only the first `maxInputPorts` ports are used.
- Up to `maxBitsPerPort` bits per port are sampled, evenly spaced across the width.

### Optimization rule
- Register bits are sampled and capped by `maxRegBits`.
- Both inputs and internal/submodule bits are hashed to fixed widths.

## Parameters
Default parameters (in `HierCovParams`):
- inputHashSize = 6
- coreHashSize = 8
- submodHashSize = 6
- covSumSize = 32
- maxInputPorts = 8
- maxBitsPerPort = 8
- maxRegBits = 64

Default parameters (in `HierCovParamsV2`):
- inputHashSize = 6
- coreHashSize = 8
- submodHashSize = 6
- covSumSize = 32
- maxInputPorts = 8
- maxBitsPerPort = 8
- maxRegBits = 64
- bucketCount = 16
- bucketWidth = 8
- maxBucketSigBits = 128
Note: `bucketCount` must be a power of 2 (>= 2).
Default parameters (in `HierCovParamsV3`): same as `HierCovParams` (v1).
Default parameters (in `HierCovParamsV4`): same as `HierCovParamsV2` (v2).

## How to use
Add the transform `hier_cov.hierCoverage`, `hier_cov.hierCoverage_v2`, `hier_cov.hierCoverage_v3`, or `hier_cov.hierCoverage_v4` to your FIRRTL transform pipeline, similar to other passes in this repository.

## Expected results
- Each module has two new output ports: `io_hierCovSum` and `io_hierCovHash`.
- Each module has a coverage memory named `<module>_hierCov` and a sum register `<module>_hierCovSum`.
- Coverage is tracked over a 2-D space of `{input_hash, core_hash}`.

## v2 notes (multi-bucket submodule signature)
In v2, each module maintains `bucketCount` coverage buckets that increment on *new* coverage hits. The bucket index is a XOR-folded hash of the coverage address. The bucket vector is then hashed to produce `io_hierCovHash`, which is exported to the parent as the submodule abstraction.
