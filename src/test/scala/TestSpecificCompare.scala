import TestUtil.solveBoth
import main.program.ProgramTemplates
import main.solver.SolverUtil
import main.solver.SolverUtil.compareSolutions
import org.scalatest.funsuite.AnyFunSuite

class TestSpecificCompare extends AnyFunSuite {

  test("Demanded Simple") {
    val queryId = 1
    val p = ProgramTemplates.DemandedSimple
    val (e, d) = solveBoth(p, queryId)
    assert(compareSolutions(e, d, queryId))
    assert(SolverUtil.solutionSize(d) < SolverUtil.solutionSize(e))
  }


  test("Transitive Token Tracking") {
    val queryId = 3
    val p = ProgramTemplates.TransitiveTokenTracking
    val (e, d) = solveBoth(p, queryId)
    assert(compareSolutions(e, d, queryId))
  }

  test("Unconditional tracking in store") {
    val queryId = 4
    val p = ProgramTemplates.UnconditionalTokenTrackingInStore
    val (e, d) = solveBoth(p, queryId)
    assert(compareSolutions(e, d, queryId))
  }

  // Shows the need for tracking tokens of the base for x.f = y, if y has tracked tokens.
  test("TrackBaseInStore") {
    val queryId = 6
    val p = ProgramTemplates.TrackBaseInStore
    val (e, d) = solveBoth(p, queryId)

    assert(compareSolutions(e, d, queryId))
  }

  test("MultipleFields") {
    val queryId = (1, "f")
    val p = ProgramTemplates.MultipleFields
    val (e, d) = solveBoth(p, queryId)

    assert(compareSolutions(e, d, queryId))
  }

  test("FunctionAndField") {
    val queryId = 4
    val p = ProgramTemplates.FunctionAndField
    val (e, d) = solveBoth(p, queryId)

    assert(compareSolutions(e, d, queryId))
  }

  test("MergeInCall") {
    val queryId = 4
    val p = ProgramTemplates.MergeInCall
    val (e, d) = solveBoth(p, queryId)

    assert(compareSolutions(e, d, queryId))
  }

  test("TrackedArgumentsMustPropagate") {
    val queryId = 4
    val p = ProgramTemplates.TrackedArgumentsMustPropagate
    val (e, d) = solveBoth(p, queryId)

    assert(compareSolutions(e, d, queryId))
  }
  test("FunctionsAndReturns") {
    val queryId = 7
    val p = ProgramTemplates.TrackedArgumentsMustPropagate
    val (e, d) = solveBoth(p, queryId)

    assert(compareSolutions(e, d, queryId))
  }

  test("MergeOfArguments") {
    val queryId = 1
    val p = ProgramTemplates.MergeOfArguments
    val (e, d) = solveBoth(p, queryId)

    assert(compareSolutions(e, d, queryId))
  }

}
