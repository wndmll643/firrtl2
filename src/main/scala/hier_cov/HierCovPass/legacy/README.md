# HierCovPass — Legacy variants

This directory contains older or experimental hierarchical-coverage passes that
have been superseded by the variants kept at the parent directory's top level
(`v6a`, `v6b`, `v9a`, `v9b`).

These passes still compile and remain addressable as
`-fct hier_cov.hierCoverage_<name>` — Scala uses package declarations rather
than directory paths, so moving them here is purely organizational. Use them
only if you have a specific reason: reproducing historical experiments,
debugging an issue you suspect originates in a particular version, or
preserving old benchmark output formats.

For the recommended variants and current results, see
`../README.md`.

## Why these are legacy

| Variant | Reason |
|---------|--------|
| v1, v2, v3 | Foundational versions superseded by v4 |
| v4, v4_3746 | Generic baselines superseded by v6a/v9a |
| v4_fix, v4_3746_fix, v5_fix | Adds `SIntLiteral(0)` metaReset fix that v9a now includes natively |
| v5 | Abandoned all-signal hashing experiment (no control/data distinction) |
| v7 | Dynamic sizing + extmodule proxy, kept v6a's noisy data-input approach |
| v7b | v7 + control-input selection. Superseded by v9 (lower coverage, slower TTB) |
| v8a | AFL-style transition coverage. **Failed** — saturates at 5 states across 84% of runs |
| v8b | v8a with control-input selection. Marginally better than v6b, still dominated by v9 |
| v9c | XOR-fold hash (no direct-concat path), v6b-style sizing. Dominated by v9a |
| v9d | XOR-fold hash, v7b-style dynamic sizing. Similar to v7b, superseded by v9b |

## Files in this directory

```
HierCovPass_v1.scala         hierCoverage         (v1)
HierCovPass_v2.scala         hierCoverage_v2      (v2)
HierCovPass_v3.scala         hierCoverage_v3      (v3)
HierCovPass_v4.scala         hierCoverage_v4      (v4)
HierCovPass_v4_fix.scala     hierCoverage_v4_fix
HierCovPass_v4_3746.scala    hierCoverage_v4_3746
HierCovPass_v4_3746_fix.scala hierCoverage_v4_3746_fix
HierCovPass_v5.scala         hierCoverage_v5
HierCovPass_v5_fix.scala     hierCoverage_v5_fix
HierCovPass_v7.scala         hierCoverage_v7
HierCovPass_v7b.scala        hierCoverage_v7b
HierCovPass_v8a.scala        hierCoverage_v8a
HierCovPass_v8b.scala        hierCoverage_v8b
HierCovPass_v9c.scala        hierCoverage_v9c
HierCovPass_v9d.scala        hierCoverage_v9d
```

## Detailed history

### v1–v4: foundation

- **v1** (`hierCoverage`): initial pass. Input hashing + reg/submodule hashing + 2-D bitmap + metaAssert/metaReset. Fixed `inputHashSize`/`coreHashSize`. Submodule hash is a single XOR-fold of child's coverage summary.
- **v2** (`hierCoverage_v2`): replaces submodule hash with **multi-bucket signature**. Each module maintains `bucketCount` coverage buckets; the bucket vector is hashed to produce `io_hierCovHash`. Better parent-visible abstraction.
- **v3** (`hierCoverage_v3`): v1 + guards for **unknown-width** ports/regs (skips sampling when width is not concrete). Needed for designs with parameterized widths.
- **v4** (`hierCoverage_v4`): v2 + unknown-width guards (v3 fix applied to v2 bucket semantics). Was the recommended baseline before v6 introduced control-flow awareness.

### v4 variants: fixes and summary reporting

- **v4_fix** (`hierCoverage_v4_fix`): v4 + **SInt metaReset fix** — uses `SIntLiteral(0)` instead of `UIntLiteral(0)` for SInt-typed registers. Was required for designs with SInt regs (e.g. `DivSqrtRawFN.sExp_Z` in FPU). **v9a now includes this fix natively, so v4_fix is no longer needed.**
- **v4_3746**: v4 + `writeCoverageSummary()` — writes `{topName}_hier_cov_summary.txt` and `summary.txt` with per-module coverage point counts.
- **v4_3746_fix**: v4_3746 + SInt metaReset fix.

### v5: all-signal hashing with raised caps

- **v5** (`hierCoverage_v5`): raised `inputHashSize=8`, `coreHashSize=10`. Uses graphLedger for instance/register discovery but does NOT use control-flow analysis — hashes **all** input ports and **all** registers (no control/data distinction). No `maxInputPorts` limit.
- **v5_fix**: v5 + SInt metaReset fix.

### v7: dynamic hash sizing + extmodule proxy

- **v7** (`hierCoverage_v7`): based on v6a (data-input approach) with three improvements.
  - **Dynamic hash sizing**: hash width scales as `max(minHashSize, min(maxCap, log2Ceil(numBits + 1)))` instead of `min(cap, numBits)`. Large modules get proportionally wider hashes. Raised caps: `maxInputHashSize=10`, `maxCoreHashSize=12`, added `minHashSize=4` floor.
  - **Raised maxRegBits**: 256 (from 64). Samples 4x more control register bits.
  - **ExtModule I/O proxy**: hashes input ports of ExtModule children as a proxy for their internal state (which is invisible). Controlled by `maxExtModPorts=16`, `maxExtModBitsPerPort=8`.
  - Writes coverage summary with extProxyBits count per module.

**v7 limitation**: keeps v6a's data-input approach for `input_hash`. The log2 scaling and extmodule proxy improve the core hash, but the input side remains noisy. On micro_boom_720, v7 saturates at 510 states by cycle ~8,661 while v6b (control inputs) sustains coverage growth to ~100K+ cycles.

### v7b: v7 with control-input selection

- **v7b** (`hierCoverage_v7b`): v7 with v6b's control-input-only selection for input hash.
  - Only change: `selectDataInputBits` → `selectControlInputBits` (`.filterNot(ctrlPortNames)` → `.filter(ctrlPortNames)`).
  - All v7 features preserved: dynamic log2 hash sizing, extmodule proxy, raised caps.

### v8a/v8b: transition coverage + cone+origin register filtering — FAILED

Both v8 variants add two improvements over v7: **cone+origin register filtering** and **AFL-style transition coverage**. They differ only in input selection strategy.

**Cone+origin register filtering** (replaces plain mux-source tracing):
- **Filter A (backward cone)**: `graphLedger.findControlConeRegs(maxOutputWidth=16)` — traces backward from narrow output ports (width <= 16) to find which registers can influence control outputs (exceptions, ready/valid, etc.).
- **Filter B (input-derived)**: `graphLedger.findInputDerivedRegs(coneRegs, maxHops=3)` — N-hop origin classification. Registers whose values trace back to input ports within 3 hops are excluded (they're buffered input data, not computed control state).
- **v8Regs = coneRegs - inputDerivedRegs**: only registers that are both in the control cone AND internally computed.

**AFL-style transition coverage**:
- Each module stores `prev_addr` (one register, same width as coverage address).
- Coverage map indexed by `transition_addr = addr XOR (prev_addr >> 1)`.
- Makes (A→B) and (B→A) distinct transitions. Hardware cost: one register + one XOR + one shift per module.
- `io_hierCovHash` (parent aggregation) stays state-based — transition tracking is local.
- Coverage map size unchanged.

- **v8a** (`hierCoverage_v8a`): cone+origin + transition + **data-input** (v6a-style) for input hash.
- **v8b** (`hierCoverage_v8b`): cone+origin + transition + **control-input** (v6b-style) for input hash.

**v8 result (failed)**: AFL-style transition coverage was the wrong abstraction for hardware. v8a saturated at exactly 5 coverage states across 84% of micro_boom_720 runs (251/300), and 4 states in the rest. Reason: in RTL, consecutive cycles differ by only 1-2 bits, so `addr XOR (prev_addr >> 1)` produces near-zero transition addresses that all collapse to the same map entries. v8b was no better than v6b (1216 vs 1118 mean coverage). The v8 line is dropped going forward.

### v9c/v9d: XOR-fold ablations

These were ablation studies for the v9 design. They isolate the effect of the better hash function (XOR-fold) without the direct-concat path, to confirm that direct concatenation is the key improvement and not just XOR-fold.

- **v9c** (`hierCoverage_v9c`): **XOR-fold only** + v6b-style fixed sizing. Tests whether the better hash function alone (without direct-concat path) is sufficient.
  - Hash function: `buildHashFold` (XOR-fold, no direct path).
  - Otherwise mirrors v9a parameters; `submodHashSize=12`.

- **v9d** (`hierCoverage_v9d`): **XOR-fold only** + v7b-style dynamic log2 sizing + extmodule proxy.
  - Hash function: `buildHashFold`.
  - Otherwise mirrors v7b architecture (dynamic log2 sizing preserved), with `submodHashSize=16`.

The experiment confirmed v9c/v9d are dominated by v9a/v9b's direct-concat path: matched pairs v9a vs v9c (8x more coverage for v9a) and v9b vs v9d (2.2x more coverage for v9b).

## Version comparison

### Signal selection strategy

| Version | Input hash source | Core hash source | Control-flow aware |
|---------|------------------|------------------|--------------------|
| v1–v4 | All input ports | All registers + child hash | No |
| v5 | All input ports | All registers + child hash | No (graphLedger for discovery only) |
| v6a | Data input ports (non-control) | Control registers only + child hash | Yes (graphLedger mux analysis) |
| v6b | Control input ports only | Control registers only + child hash | Yes |
| v7 | Data input ports (non-control) | Control registers + child hash + extmodule proxy | Yes |
| v7b | Control input ports only | Control registers + child hash + extmodule proxy | Yes |
| v8a | Data input ports (non-control) | Cone+origin regs + child hash + extmodule proxy | Yes (cone+origin + transition) |
| v8b | Control input ports only | Cone+origin regs + child hash + extmodule proxy | Yes (cone+origin + transition) |
| v9a | Control input ports only | Control regs + wide child hash (16b) | Yes |
| v9b | Control input ports only | Control regs + wide child hash (16b) + extmodule proxy | Yes |
| v9c | Control input ports only | Control regs + child hash (12b) | Yes |
| v9d | Control input ports only | Control regs + wide child hash (16b) + extmodule proxy | Yes |

### Register filtering strategy

| Version | Register filter | Method |
|---------|----------------|--------|
| v1–v5 | All registers | None |
| v6a, v6b, v7, v7b | Mux-source registers | `graphLedger.findMuxSrcs` → `ctrlRegs` |
| v8a, v8b | Cone ∩ ¬InputDerived | `graphLedger.findControlConeRegs` − `findInputDerivedRegs` → `v8Regs` |
| v9a, v9b, v9c, v9d | Mux-source registers (same as v6) | `graphLedger.findMuxSrcs` → `ctrlRegs` |

### Hash sizing strategy

| Version | Input hash width | Core hash width |
|---------|-----------------|-----------------|
| v1–v4 | Fixed (`inputHashSize`, default 6) | Fixed (`coreHashSize`, default 8) |
| v5 | Fixed 8 | Fixed 10 |
| v6a, v6b | `min(6, numBits)` | `min(6, numBits)` |
| v7, v7b, v8a, v8b | `max(4, min(10, log2(numBits)))` | `max(4, min(12, log2(numBits)))` |
| v9a | `min(6, numBits)` | `min(14, numBits)` (capped by `maxAddrWidth=20`) |
| v9b | `max(4, min(10, numBits))` | `max(4, min(14, numBits))` (capped by `maxAddrWidth=20`) |
| v9c | `min(6, numBits)` | `min(14, numBits)` (capped by `maxAddrWidth=20`) |
| v9d | `max(4, min(10, log2(numBits)))` | `max(4, min(14, log2(numBits)))` |

### Parameters by version

| Parameter | v1/v3 | v2/v4 | v5 | v6a | v6b | v7/v7b | v8a/v8b | v9a | v9b | v9c | v9d |
|-----------|-------|-------|----|-----|-----|--------|---------|-----|-----|-----|-----|
| inputHashSize / maxInputHashSize | 6 | 6 | 8 | 6 | 6 | 10 | 10 | 6 | 10 | 6 | 10 |
| coreHashSize / maxCoreHashSize | 8 | 8 | 10 | 6 | 6 | 12 | 12 | 14 | 14 | 14 | 14 |
| minHashSize | — | — | — | — | — | 4 | 4 | — | 4 | — | 4 |
| maxAddrWidth (new) | — | — | — | — | — | — | — | 20 | 20 | 20 | — |
| submodHashSize | 6 | 6 | 6 | 6 | 6 | 6 | 6 | 16 | 16 | 12 | 16 |
| maxInputPorts | 8 | 8 | — | 8 | 8 | 8 | 8 | 8 | 8 | 8 | 8 |
| maxBitsPerPort | 8 | 8 | 8 | 8 | 8 | 8 | 8 | 8 | 8 | 8 | 8 |
| maxRegBits | 64 | 64 | 64 | 64 | 64 | 256 | 256 | 64 | 256 | 64 | 256 |
| maxCtrlRegWidth | — | — | — | 20 | 20 | 20 | 20 | 20 | 20 | 20 | 20 |
| bucketCount | — | 16 | 16 | 16 | 16 | 16 | 16 | 16 | 32 | 16 | 16 |
| bucketWidth | — | 8 | 8 | 8 | 8 | 8 | 8 | 8 | 8 | 8 | 8 |
| maxBucketSigBits | — | 128 | 128 | 128 | 128 | 128 | 128 | 128 | 256 | 128 | 128 |
| maxExtModPorts | — | — | — | — | — | 16 | 16 | — | 16 | — | 16 |
| maxExtModBitsPerPort | — | — | — | — | — | 8 | 8 | — | 8 | — | 8 |
| maxControlOutputWidth | — | — | — | — | — | — | 16 | — | — | — | — |
| maxOriginHops | — | — | — | — | — | — | 3 | — | — | — | — |
| Hash function | bucket XOR-reduce | bucket XOR-reduce | bucket XOR-reduce | bucket XOR-reduce | bucket XOR-reduce | bucket XOR-reduce | bucket XOR-reduce | direct-or-fold | direct-or-fold | XOR-fold | XOR-fold |
