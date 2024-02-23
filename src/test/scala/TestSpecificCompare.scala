import TestUtil.solveBoth
import main.program.{Program, ProgramTemplates}
import main.solver.SolverUtil.compareSolutions
import main.solver.{QueryID, SolverUtil}
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
    assertEqualSolution(ProgramTemplates.TransitiveTokenTracking, 3)
  }

  test("Unconditional tracking in store") {
    assertEqualSolution(ProgramTemplates.UnconditionalTokenTrackingInStore, 4)
  }

  // Shows the need for tracking tokens of the base for x.f = y, if y has tracked tokens.
  test("TrackBaseInStore") {
    assertEqualSolution(ProgramTemplates.TrackBaseInStore, 6)
  }

  test("MultipleFields") {
    assertEqualSolution(ProgramTemplates.MultipleFields, (1, "f"))
  }

  test("FunctionAndField") {
    assertEqualSolution(ProgramTemplates.FunctionAndField, 4)
  }

  test("MergeInCall") {
    assertEqualSolution(ProgramTemplates.MergeInCall, 4)
  }

  test("TrackedArgumentsMustPropagate") {
    assertEqualSolution(ProgramTemplates.TrackedArgumentsMustPropagate, 4)

  }
  test("FunctionsAndReturns") {
    assertEqualSolution(ProgramTemplates.FunctionsAndReturns, 7)
  }

  test("MergeOfArguments") {
    assertEqualSolution(ProgramTemplates.MergeOfArguments, 1)
  }

  test("TrackedRetNodeImpliesTrackFunction") {
    assertEqualSolution(ProgramTemplates.TrackedRetNodeImpliesTrackFunction, 0)
  }


  private def assertEqualSolution(p: Program, query: QueryID): Unit = {
    val (e, d) = solveBoth(p, query)
    assert(compareSolutions(e, d, query))
  }


}
