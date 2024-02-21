import TestUtil.{newDist, newGenerator, repeatTest, solveBoth}
import main.constraint.ConstraintVariables
import main.program.*
import main.solver.SolverUtil
import main.solver.SolverUtil.compareSolutions
import org.scalatest.funsuite.AnyFunSuite


class TestComparison extends AnyFunSuite {

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


  test("Small programs") {
    repeatTest(10000, newGenerator(5, 2, 15, newDist(20, 50, 20, 10)))
  }

  test("Medium Load/Store heavy") {
    repeatTest(10000, newGenerator(7, 3, 10, newDist(20, 20, 30, 30)))
  }

  test("Medium programs") {
    repeatTest(10000, newGenerator(10, 3, 25, newDist(20, 50, 20, 10)))
  }

  test("Random program") {
    repeatTest(1000, newGenerator(100, 30, 200, newDist(20, 50, 20, 10)))
  }

  test("Load/Store heavy") {
    repeatTest(200, newGenerator(250, 50, 500, newDist(25, 25, 25, 25)))
  }

  test("Large") {
    repeatTest(100, newGenerator(100, 20, 5000, newDist(25, 35, 20, 20)))
  }

  //  test("Function call programs") {
  //    repeatTest(5000, newGenerator(7, 3, 250, newDist(15, 25, 10, 10, 20, 20)))
  //  }
}
