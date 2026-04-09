# HierCovPass

Hierarchical coverage transform for FIRRTL. Each module is instrumented to:
- Build a coverage address from `{input_hash, core_hash}`.
- Maintain a 1-bit coverage map indexed by that address.
- Expose `io_hierCovSum` (running count of unique map hits) and
  `io_hierCovHash` (compact signature for parent aggregation).

Submodules' `io_hierCovHash` outputs are folded back into the parent's
`core_hash`, so coverage information propagates up the module hierarchy.

The pass also adds `metaAssert` aggregation and `metaReset` wiring for
test-only state initialization.

## Recommended variants

**Use `hierCoverage_v9a` unless you have a specific reason not to.** It is
the fastest time-to-bug variant on every benchmark we've run, including
beating DifuzzRTL register coverage on the hard micro_boom_720 LSU bug.

Four variants are kept at the top level of this directory:

| Variant | When to use | File |
|---|---|---|
| **`hierCoverage_v9a`** | **Default — fastest TTB, recommended for all new work** | `HierCovPass_v9a.scala` |
| `hierCoverage_v6a` | DifuzzRTL-style data-input baseline; required by encarsia/hierfuzz Yosys flow for compatibility | `HierCovPass_v6a.scala` |
| `hierCoverage_v6b` | Stable control-input reference; second-best TTB | `HierCovPass_v6b.scala` |
| `hierCoverage_v9b` | Maximum coverage exploration on large designs (warning: slower TTB than v9a due to corpus bloat) | `HierCovPass_v9b.scala` |

Everything else is in `legacy/` — see the "Legacy variants" section below.

### Why v9a wins

v9a's advantage comes from three design choices working together:

1. **Direct concatenation eliminates hashing collisions.** Older variants
   (v1–v8) use a bucket XOR-reduce hash that collapses each bucket of N bits
   to a single parity bit, causing ~50% collision probability per bucket.
   v9a's `buildDirectOrFold` directly concatenates all sampled bits when they
   fit within the address width (`maxAddrWidth=20`, i.e. 1M coverage map
   entries) — zero information loss. It only falls back to XOR-fold hashing
   when bits exceed that width.

2. **Fixed, moderate sizing prevents coverage explosion.** v9a uses
   `min(numBits, cap)` sizing with `maxInputHashSize=6` and
   `maxCoreHashSize=14`, giving each module a coverage map large enough to
   distinguish meaningful states but small enough that the fuzzer doesn't
   drown in trivial state differences. Variants with dynamic sizing (v9b,
   v7b) generate too many "interesting" inputs and bloat the corpus.

3. **Wider hierarchy pipe (`submodHashSize=16`).** v6/v7 used a 6-bit
   `io_hierCovHash`, meaning each child could only pass 64 distinct state
   signatures to its parent. v9a uses 16 bits (65,536 signatures), recovering
   child-module information that older variants threw away.

### Experimental results

200 runs on micro_boom_720 (LSU bug — the hard benchmark):

| Variant | Mean TTB (cycles) | Mean coverage |
|---|---|---|
| **v9a** | **275,451** | 8,924 |
| v6b | 294,426 | 1,118 |
| reg (DifuzzRTL) | 299,394 | — |
| v6a | 297,019 | 1,123 |
| rand | 343,994 | — |
| v9b | 346,108 | 32,835 |

v9a is 6.4% faster than v6b and 20% faster than random fuzzing. v9b has the
highest absolute coverage but ranks 7th in TTB, confirming that
fine-grained coverage tracking does not directly translate to faster
bug-finding. See `../../../../../micro_boom/v9_results_report.md` for the
full report.

## How it works

Each module computes:

1. **`input_hash`**: hash of selected input bits — control input ports for
   v6b/v9a/v9b, data input ports for v6a.
2. **`core_hash`**: hash of internal control register bits + each child
   submodule's `io_hierCovHash`.
3. **Coverage bitmap** indexed by `{input_hash, core_hash}`.
4. **`io_hierCovSum`** (32-bit) and **`io_hierCovHash`** (16-bit for v9, 6-bit
   for v6) outputs.

For v9a specifically, the address-width calculation uses `min(numBits, cap)`
and is hard-capped at `maxAddrWidth=20`, so the coverage map never exceeds
2^20 = 1M entries per module regardless of how many sampled bits the module
has. The hash function `buildDirectOrFold` produces zero-loss output when
`numBits <= addrWidth` and falls back to XOR-fold otherwise.

For the gory details of every historical variant and the v8 transition-coverage
failure, see `legacy/README.md`.

## Parameters (v9a defaults)

| Parameter | Value | Meaning |
|---|---|---|
| `maxInputHashSize` | 6 | Max bits used for input portion of address |
| `maxCoreHashSize` | 14 | Max bits used for core (regs + child hash) portion |
| `maxAddrWidth` | 20 | Hard cap on total address width (2^20 = 1M map entries) |
| `submodHashSize` | 16 | Width of `io_hierCovHash` propagated to parents |
| `maxInputPorts` | 8 | Sample at most this many input ports per module |
| `maxBitsPerPort` | 8 | Sample at most this many bits from each port |
| `maxRegBits` | 64 | Sample at most this many control register bits |
| `maxCtrlRegWidth` | 20 | Skip control registers wider than this |
| `bucketCount` | 16 | Number of bucket counters for `io_hierCovHash` signature |
| `bucketWidth` | 8 | Width of each bucket counter |

## How to use

Add the transform to your FIRRTL pipeline via `-fct`:

```bash
firrtl2/utils/bin/firrtl -td <build_dir> -i <low.lo.fir> \
  -fct hier_cov.hierCoverage_v9a -X verilog -o <out.v>
```

Available passes (top-level — see `legacy/README.md` for the rest):

- **`hier_cov.hierCoverage_v9a`** — recommended default (direct-or-fold, fixed sizing)
- `hier_cov.hierCoverage_v6a` — DifuzzRTL data-input baseline
- `hier_cov.hierCoverage_v6b` — control-input baseline (XOR-reduce hash)
- `hier_cov.hierCoverage_v9b` — direct-or-fold with dynamic sizing + extmodule proxy (max coverage)

## Expected output

- Each module gets two new output ports: `io_hierCovSum` (32-bit) and
  `io_hierCovHash` (16-bit for v9a/v9b, 6-bit for v6a/v6b).
- Each module gets a coverage memory `<module>_hierCov` and a sum register
  `<module>_hierCovSum`.
- v6a+ versions write `{topName}_hier_cov_summary.txt` with per-module
  coverage stats.

## Legacy variants

The 15 older / experimental variants live in [`legacy/`](legacy/). They still
compile and remain addressable as `-fct hier_cov.hierCoverage_<name>` because
Scala packages are determined by source declaration, not directory path.
Use them only for reproducing historical experiments.

Quick reference:

| Family | Variants | Status |
|---|---|---|
| Foundational | v1, v2, v3, v4, v4_3746 | Superseded by v6/v9 |
| SInt-fixed older | v4_fix, v4_3746_fix, v5_fix | v9a now has the fix natively |
| All-signal hashing | v5 | Abandoned experiment |
| Dynamic sizing + extmod | v7, v7b | Superseded by v9 |
| AFL-style transition coverage | v8a, v8b | **Failed** (saturates at 5 states) |
| XOR-fold ablations | v9c, v9d | Dominated by v9a/v9b |

See [`legacy/README.md`](legacy/README.md) for full historical detail and
parameter comparison tables.
