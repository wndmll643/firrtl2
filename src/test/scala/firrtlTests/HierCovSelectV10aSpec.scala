// SPDX-License-Identifier: Apache-2.0
// Sanity tests for hier_cov.hierCoverage_v10a and the
// HierCovSelectionAnnotation contract.
package firrtlTests

import firrtl2._
import firrtl2.stage.{FirrtlSourceAnnotation, FirrtlStage, RunFirrtlTransformAnnotation}
import firrtl2.testutils.FirrtlFlatSpec

import hier_cov.{HierCovSelectionAnnotation, ModuleSelection, hierCoverage_v10a}

class HierCovSelectV10aSpec extends FirrtlFlatSpec {

  // A tiny module with two regs, one mux, one output port. Enough to exercise
  // v10a's selection path without pulling in a real core. Explicit reset
  // clauses on both registers so the FIRRTL legality check is satisfied on
  // every version of the toolchain (some versions warn/fail on registers
  // without reset in Low FIRRTL).
  private val input =
    """|circuit M :
       |  module M :
       |    input  clock : Clock
       |    input  reset : UInt<1>
       |    input  sel   : UInt<1>
       |    output out   : UInt<8>
       |
       |    reg r0 : UInt<8>, clock with : (reset => (reset, UInt(0)))
       |    reg r1 : UInt<8>, clock with : (reset => (reset, UInt(0)))
       |    r0 <= r0
       |    r1 <= r1
       |    out <= mux(sel, r0, r1)
       |""".stripMargin

  behavior.of("hierCoverage_v10a")

  it should "run successfully when a HierCovSelectionAnnotation is provided" in {
    val selection = HierCovSelectionAnnotation(
      version = 1,
      modules = Map(
        "M" -> ModuleSelection(
          regs  = Map("r0" -> Seq(0, 1, 2, 3)),
          ports = Map("sel" -> Seq(0))
        )
      )
    )
    val result = (new FirrtlStage).execute(
      Array.empty,
      Seq(
        FirrtlSourceAnnotation(input),
        selection,
        RunFirrtlTransformAnnotation(new hierCoverage_v10a)
      )
    )
    // If we got here, v10a ran without the missing-annotation error. Confirm
    // the stage produced some output.
    assert(result.nonEmpty)
  }

  it should "raise FirrtlUserException when no HierCovSelectionAnnotation is present" in {
    val ex = intercept[Exception] {
      (new FirrtlStage).execute(
        Array.empty,
        Seq(
          FirrtlSourceAnnotation(input),
          RunFirrtlTransformAnnotation(new hierCoverage_v10a)
        )
      )
    }
    // FirrtlStage wraps user exceptions; find the originating cause.
    val message = Iterator.iterate[Throwable](ex)(_.getCause).takeWhile(_ != null).map(_.getMessage).mkString(" | ")
    message should include("hierCoverage_v10a requires exactly one HierCovSelectionAnnotation")
  }
}
