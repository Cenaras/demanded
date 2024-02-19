import main.constraint.{ConstraintGenerator, ConstraintVariables}
import main.program.*
import main.solver.SolverUtil.compareSolutions
import main.solver.{ExhaustiveSolver, HTSolver, QueryID, SolverUtil}
import main.util.PrettyPrinter
import org.scalatest.funsuite.AnyFunSuite


class TestComparison extends AnyFunSuite {

  test("Demanded Simple") {
    val queryId = 1
    val p = ProgramTemplates.demandedSimple
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


  def repeatTest(times: Int, generator: ProgramGenerator): Unit = {
    for (i <- 0 until times) {
      val program = generator.generate()
      val query = program.getRandomVar

      val (exhaustiveSolution, demandedSolution) = solveBoth(program, query)
      if (!compareSolutions(exhaustiveSolution, demandedSolution, query)) {
        throw Error("Solutions did not match with query %d for program\n%s".format(query, PrettyPrinter.stringifyProgram(program)))
      }

      val demandedSolutionSize = SolverUtil.solutionSize(demandedSolution)
      val exhaustiveSolutionSize = SolverUtil.solutionSize(exhaustiveSolution)
      assert(demandedSolutionSize <= exhaustiveSolutionSize)
    }
  }

  def solveBoth(p: Program, queryId: QueryID): (ConstraintVariables, ConstraintVariables) = {
    val eConstraints = ConstraintGenerator.generate(p)
    val dConstraints = ConstraintGenerator.generate(p)

    val exhaustive = ExhaustiveSolver()
    val ht = new HTSolver()

    val exhaustiveSolution = exhaustive.solve(eConstraints)
    val demandedSolution = ht.solve(dConstraints, queryId)
    (exhaustiveSolution, demandedSolution)
  }

  def newGenerator(varNumber: Int, tokenNum: Int, size: Int, dist: ProgramDistribution): ProgramGenerator = {
    new ProgramGenerator(varNumber, tokenNum, size, dist)
  }

  def newDist(newObj: Int, assign: Int, load: Int, store: Int) = {
    new ProgramDistribution(newObj, assign, load, store)
  }

}
