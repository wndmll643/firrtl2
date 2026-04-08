# HierCovPass

Hierarchical coverage transform for FIRRTL. This pass adds:
- Hierarchical coverage collection with a 2-D bitmap (flattened address = input_hash ++ core_hash).
- Submodule abstraction via hashing of each submodule's coverage summary.
- metaAssert aggregation and metaReset wiring (same semantics as the coverage pass).

## How it works
Each module computes:
1) `input_hash`: hash of selected input bits (limited by params).
2) `core_hash`: hash of internal register bits plus submodule coverage hashes.
3) Coverage bitmap indexed by `{input_hash, core_hash}` (flattened into one address).
4) `io_hierCovSum` (coverage hit counter) and `io_hierCovHash` (hash of bucketed coverage signature).

Submodules export `io_hierCovHash`, which is folded into the parent `core_hash`.

## Files and versions

### v1–v4: foundation

- **v1** (`hierCoverage`): initial pass. Input hashing + reg/submodule hashing + 2-D bitmap + metaAssert/metaReset. Fixed `inputHashSize`/`coreHashSize`. Submodule hash is a single XOR-fold of child's coverage summary.
- **v2** (`hierCoverage_v2`): replaces submodule hash with **multi-bucket signature**. Each module maintains `bucketCount` coverage buckets; the bucket vector is hashed to produce `io_hierCovHash`. Better parent-visible abstraction.
- **v3** (`hierCoverage_v3`): v1 + guards for **unknown-width** ports/regs (skips sampling when width is not concrete). Needed for designs with parameterized widths.
- **v4** (`hierCoverage_v4`): v2 + unknown-width guards (v3 fix applied to v2 bucket semantics). **Preferred baseline for most designs.**

### v4 variants: fixes and summary reporting

- **v4_fix** (`hierCoverage_v4_fix`): v4 + **SInt metaReset fix** — uses `SIntLiteral(0)` instead of `UIntLiteral(0)` for SInt-typed registers. Required for designs with SInt regs (e.g. `DivSqrtRawFN.sExp_Z` in FPU).
- **v4_3746**: v4 + `writeCoverageSummary()` — writes `{topName}_hier_cov_summary.txt` and `summary.txt` with per-module coverage point counts.
- **v4_3746_fix**: v4_3746 + SInt metaReset fix. Same summary output.

### v5: all-signal hashing with raised caps

- **v5** (`hierCoverage_v5`): raised `inputHashSize=8`, `coreHashSize=10`. Uses graphLedger for instance/register discovery but does NOT use control-flow analysis — hashes **all** input ports and **all** registers (no control/data distinction). No `maxInputPorts` limit.
- **v5_fix**: v5 + SInt metaReset fix.

### v6a/v6b: control-flow aware signal selection (via graphLedger)

Both v6a and v6b use DifuzzRTL's `graphLedger` to identify **control sources** — signals that feed mux select inputs. The graphLedger traces backwards from each `Mux.cond` to classify registers as control regs (`ctrlRegs`) and input ports as control ports (`ctrlPortNames`). This separates control-flow state from data/operand state.

- **v6a** (`hierCoverage_v6a`): based on v4_3746_fix.
  - **Core hash**: uses **control registers only** (`selectControlRegBits`) — registers that feed mux selects. Excludes directly-input-fed regs (width > 3) and wide regs (>= 20 bits).
  - **Input hash**: uses **data input ports** — input ports that do NOT feed mux selects (`selectDataInputBits`, `.filterNot(ctrlPortNames)`).
  - Hash sizing: `min(cap, numBits)` with `maxInputHashSize=6`, `maxCoreHashSize=6`.
  - Includes SInt fix + writeCoverageSummary.

- **v6b** (`hierCoverage_v6b`): based on v6a. Single change in input selection.
  - **Core hash**: same as v6a (control registers only).
  - **Input hash**: uses **control input ports only** — input ports that DO feed mux selects (`selectControlInputBits`, `.filter(ctrlPortNames)`). Fully focused on control-flow state both internally and externally.
  - Hash sizing: same as v6a (`min(cap, numBits)`, caps at 6). Control inputs are naturally few and narrow, so the input hash auto-sizes small.

**v6a vs v6b key difference**: v6a hashes data inputs (wide, noisy, change every cycle), v6b hashes control inputs (narrow, sparse, change on control-flow transitions). On micro_boom_720 (LSU), v6b produces denser, slower-saturating coverage and is the best-performing hier variant.

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

### v8a/v8b: transition coverage + cone+origin register filtering

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

v8a vs v8b isolates whether input selection strategy matters when combined with improved register filtering and transition tracking.

**v8 result (failed)**: AFL-style transition coverage was the wrong abstraction for hardware. v8a saturated at exactly 5 coverage states across 84% of micro_boom_720 runs (251/300), and 4 states in the rest. Reason: in RTL, consecutive cycles differ by only 1-2 bits, so `addr XOR (prev_addr >> 1)` produces near-zero transition addresses that all collapse to the same map entries. v8b was no better than v6b (1216 vs 1118 mean coverage). The v8 line is dropped going forward.

### v9a/v9b/v9c/v9d: direct-concat or XOR-fold (replace bucket XOR-reduce)

The v9 family targets the root cause that bottlenecked v6/v7/v8: the bucket XOR-reduce hash in `buildHash` collapses each bucket of N bits to a single parity bit, causing ~50% collision probability per bucket. v9 introduces two replacement hash functions:

- **`buildHashFold`** (XOR-fold): split N concatenated input bits into chunks of `outWidth`, then XOR the chunks position-by-position. Preserves positional information within each chunk; only bits exactly `outWidth` apart can cancel.
- **`buildDirectOrFold`** (direct-or-fold): if `numBits <= outWidth`, **directly concatenate** all bits with zero-padding (zero information loss). If `numBits > outWidth`, fall back to `buildHashFold`. Most modules in real designs fit within `maxAddrWidth=20`, so the direct path activates often.

All v9 variants also widen `submodHashSize` from 6 to 12-16 bits, recovering child-module information that v6/v7 discarded. A new `maxAddrWidth=20` cap (1M coverage map entries) bounds memory growth for large modules.

- **v9a** (`hierCoverage_v9a`): **direct-or-fold** + v6b-style fixed sizing. Maps to v6b's architecture with the bucket hash swapped out.
  - Hash function: `buildDirectOrFold` (direct concat when bits fit, XOR-fold otherwise).
  - Sizing: `min(numBits, cap)` — no log2, no dynamic scaling. `maxInputHashSize=6`, `maxCoreHashSize=14` (widened from v6b's 8).
  - `submodHashSize=16` (widened from v6b's 6) — children pass 2.7x more state to parents.
  - `maxAddrWidth=20` cap; `bucketCount=16`.
  - No extmodule proxy (matches v6b).

- **v9b** (`hierCoverage_v9b`): **direct-or-fold** + v7b-style dynamic sizing + extmodule proxy + 32 buckets.
  - Hash function: `buildDirectOrFold`.
  - Sizing: `min(numBits, cap)` with `minHashSize=4` floor — preserves more bits than v7b's `log2(numBits)` compression. `maxInputHashSize=10`, `maxCoreHashSize=14`.
  - `submodHashSize=16`, `bucketCount=32` (doubled), `maxBucketSigBits=256` (doubled).
  - `maxAddrWidth=20` cap.
  - Includes extmodule input port proxy from v7b.

- **v9c** (`hierCoverage_v9c`): **XOR-fold only** + v6b-style fixed sizing. Tests whether the better hash function alone (without direct-concat path) is sufficient.
  - Hash function: `buildHashFold` (XOR-fold, no direct path).
  - Otherwise mirrors v9a parameters; `submodHashSize=12`.

- **v9d** (`hierCoverage_v9d`): **XOR-fold only** + v7b-style dynamic log2 sizing + extmodule proxy.
  - Hash function: `buildHashFold`.
  - Otherwise mirrors v7b architecture (dynamic log2 sizing preserved), with `submodHashSize=16`.

**v9 results (200 runs each on micro_boom_720, the hard LSU bug)**:

| Variant | Mean TTB (cycles) | Mean coverage | Notes |
|---|---|---|---|
| **v9a** | **275,451** | 8,924 | **Fastest TTB across all variants — winner** |
| v6b | 294,426 | 1,118 | Previous TTB best (8x less coverage than v9a) |
| reg (DifuzzRTL) | 299,394 | — | Reference baseline |
| v9c | 313,482 | 3,325 | XOR-fold without direct path: worse than v9a |
| v9d | 327,047 | 15,082 | XOR-fold + dynamic sizing: similar to v7b |
| rand | 343,994 | — | Random baseline |
| v9b | 346,108 | **32,835** | Highest coverage but slow TTB (corpus bloat) |
| v7b | 355,923 | 15,380 | Previous coverage best |

Key takeaways from the v9 experiment:

1. **v9a is the best overall** — fastest mean TTB across all variants including DifuzzRTL's register coverage. It finds the LSU bug 6.4% faster than v6b and 20% faster than random fuzzing. The combination of direct-concat hashing + fixed moderate sizing + wider hierarchy pipe (`submodHashSize=16`) hits the right granularity sweet spot.
2. **Direct-or-fold beats XOR-fold-only**: matched pairs v9a vs v9c (8x more coverage for v9a) and v9b vs v9d (2.2x more coverage for v9b) prove that the direct-concat path is the crucial improvement, not the XOR-fold alone.
3. **More coverage hurts TTB**: v9b has 3.7x more coverage than v9a but ranks 7th in TTB — worse than random. Excessively fine-grained coverage causes corpus bloat with near-duplicate seeds.
4. **Fixed sizing > dynamic sizing for bug-finding**: v9a (fixed) beats v9b (dynamic) on TTB, and v9c beats v9d on TTB. Dynamic sizing helps coverage growth but doesn't help reach bug-triggering states faster.

## What survived (current best variants)

After running all variants (v1 through v9), only two are recommended for active use:

- **v9a — primary recommendation.** Fastest time-to-bug on the hard benchmark (micro_boom_720), beating v6b, v7b, DifuzzRTL register coverage, and random. Also produces 8x more coverage than v6b. The direct-or-fold hash with fixed `min(numBits, cap)` sizing avoids the bucket-collision bottleneck without bloating the corpus. Use this for default hierarchical-coverage fuzzing.
- **v6b — proven baseline.** The best of the bucket-XOR-reduce family. Still useful as a stable, well-understood reference and as a sanity check when comparing v9 against the prior generation. Second-best TTB (294K cycles).

Variants to avoid:
- **v8a/v8b**: transition coverage doesn't work in hardware (v8a saturates at 5 states).
- **v9b**: too much coverage, corpus bloat hurts TTB.
- **v9c/v9d**: XOR-fold-only is dominated by v9a/v9b's direct-concat path.
- **v6a, v7, v7b**: superseded by v9a. v7b coverage growth is matched by v9d but with worse TTB.

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
| **v9a** | **Control input ports only** | **Control regs + wide child hash (16b)** | **Yes** |
| v9b | Control input ports only | Control regs + wide child hash (16b) + extmodule proxy | Yes |
| v9c | Control input ports only | Control regs + child hash (12b) | Yes |
| v9d | Control input ports only | Control regs + wide child hash (16b) + extmodule proxy | Yes |

### Register filtering strategy

| Version | Register filter | Method |
|---------|----------------|--------|
| v1–v5 | All registers | None |
| v6a, v6b, v7, v7b | Mux-source registers | `graphLedger.findMuxSrcs` → `ctrlRegs` |
| v8a, v8b | Cone ∩ ¬InputDerived | `graphLedger.findControlConeRegs` − `findInputDerivedRegs` → `v8Regs` |
| **v9a, v9b, v9c, v9d** | **Mux-source registers** (same as v6) | `graphLedger.findMuxSrcs` → `ctrlRegs` |

### Hash sizing strategy

| Version | Input hash width | Core hash width |
|---------|-----------------|-----------------|
| v1–v4 | Fixed (`inputHashSize`, default 6) | Fixed (`coreHashSize`, default 8) |
| v5 | Fixed 8 | Fixed 10 |
| v6a, v6b | `min(6, numBits)` | `min(6, numBits)` |
| v7, v7b, v8a, v8b | `max(4, min(10, log2(numBits)))` | `max(4, min(12, log2(numBits)))` |
| **v9a** | `min(6, numBits)` | `min(14, numBits)` (capped by `maxAddrWidth=20`) |
| v9b | `max(4, min(10, numBits))` | `max(4, min(14, numBits))` (capped by `maxAddrWidth=20`) |
| v9c | `min(6, numBits)` | `min(14, numBits)` (capped by `maxAddrWidth=20`) |
| v9d | `max(4, min(10, log2(numBits)))` | `max(4, min(14, log2(numBits)))` |

### Parameters by version

| Parameter | v1/v3 | v2/v4 | v5 | v6a | v6b | v7/v7b | v8a/v8b | **v9a** | v9b | v9c | v9d |
|-----------|-------|-------|----|-----|-----|--------|---------|-----|-----|-----|-----|
| inputHashSize / maxInputHashSize | 6 | 6 | 8 | 6 | 6 | 10 | 10 | **6** | 10 | 6 | 10 |
| coreHashSize / maxCoreHashSize | 8 | 8 | 10 | 6 | 6 | 12 | 12 | **14** | 14 | 14 | 14 |
| minHashSize | — | — | — | — | — | 4 | 4 | **—** | 4 | — | 4 |
| maxAddrWidth (new) | — | — | — | — | — | — | — | **20** | 20 | 20 | — |
| submodHashSize | 6 | 6 | 6 | 6 | 6 | 6 | 6 | **16** | 16 | 12 | 16 |
| maxInputPorts | 8 | 8 | — | 8 | 8 | 8 | 8 | **8** | 8 | 8 | 8 |
| maxBitsPerPort | 8 | 8 | 8 | 8 | 8 | 8 | 8 | **8** | 8 | 8 | 8 |
| maxRegBits | 64 | 64 | 64 | 64 | 64 | 256 | 256 | **64** | 256 | 64 | 256 |
| maxCtrlRegWidth | — | — | — | 20 | 20 | 20 | 20 | **20** | 20 | 20 | 20 |
| bucketCount | — | 16 | 16 | 16 | 16 | 16 | 16 | **16** | 32 | 16 | 16 |
| bucketWidth | — | 8 | 8 | 8 | 8 | 8 | 8 | **8** | 8 | 8 | 8 |
| maxBucketSigBits | — | 128 | 128 | 128 | 128 | 128 | 128 | **128** | 256 | 128 | 128 |
| maxExtModPorts | — | — | — | — | — | 16 | 16 | **—** | 16 | — | 16 |
| maxExtModBitsPerPort | — | — | — | — | — | 8 | 8 | **—** | 8 | — | 8 |
| maxControlOutputWidth | — | — | — | — | — | — | 16 | **—** | — | — | — |
| maxOriginHops | — | — | — | — | — | — | 3 | **—** | — | — | — |
| Hash function | bucket XOR-reduce | bucket XOR-reduce | bucket XOR-reduce | bucket XOR-reduce | bucket XOR-reduce | bucket XOR-reduce | bucket XOR-reduce | **direct-or-fold** | direct-or-fold | XOR-fold | XOR-fold |

## How to use
Add the transform to your FIRRTL pipeline via `-fct`:

**Recommended default**: `hier_cov.hierCoverage_v9a` — fastest time-to-bug across all variants. Use v6b as a stable reference baseline if you need to compare against the prior generation.

All available passes:
- `hier_cov.hierCoverage` (v1), `hier_cov.hierCoverage_v2`, `hier_cov.hierCoverage_v3`, `hier_cov.hierCoverage_v4`
- `hier_cov.hierCoverage_v4_fix` (v4 + SInt fix)
- `hier_cov.hierCoverage_v6a`, `hier_cov.hierCoverage_v6b`
- `hier_cov.hierCoverage_v7`, `hier_cov.hierCoverage_v7b`
- `hier_cov.hierCoverage_v8a`, `hier_cov.hierCoverage_v8b` *(deprecated — transition coverage failed in hardware)*
- **`hier_cov.hierCoverage_v9a`** *(recommended)*, `hier_cov.hierCoverage_v9b`, `hier_cov.hierCoverage_v9c`, `hier_cov.hierCoverage_v9d`

## Expected results
- Each module gets two new output ports: `io_hierCovSum` (32-bit) and `io_hierCovHash`.
- Each module gets a coverage memory `<module>_hierCov` and a sum register `<module>_hierCovSum`.
- v6a+ versions write `{topName}_hier_cov_summary.txt` with per-module coverage stats.
