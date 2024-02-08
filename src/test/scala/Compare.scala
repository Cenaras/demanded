import main.constraint.{ConstraintGenerator, ConstraintVariables}
import main.program.*
import main.solver.{ExhaustiveSolver, HTSolver, QueryID}
import main.util.PrettyPrinter
import org.scalatest.funsuite.AnyFunSuite


class Compare extends AnyFunSuite {

  test("Demanded Simple") {
    val queryId = 1
    val p = ProgramTemplates.demandedSimple
    val (e, d) = solveBoth(p, queryId)
    assert(Util.assertSolutions(e, d, queryId))
    assert(SolverUtil.solutionSize(d) < SolverUtil.solutionSize(e))
  }


  test("Transitive Token Tracking") {
    val queryId = 3
    val p = ProgramTemplates.TransitiveTokenTracking
    val (e, d) = solveBoth(p, queryId)
    assert(Util.assertSolutions(e, d, queryId))
  }

  test("XXX") {
    val queryId = 4
    val p = ProgramTemplates.XXX
    val (e, d) = solveBoth(p, queryId)

    println(PrettyPrinter.printSolution(d))
    println(PrettyPrinter.printSolution(e))

    assert(Util.assertSolutions(e, d, queryId))
  }


  test("Small programs") {
    repeatTest(10000, newGenerator(5, 2, 15, (20, 50, 20, 10)))
  }


  test("Medium programs") {
    repeatTest(10000, newGenerator(10, 3, 25, (20, 50, 20, 10)))
  }

  test("Random program") {
    repeatTest(100, newGenerator(100, 30, 200, (20, 50, 20, 10)))
  }

  test("Load/Store heavy") {
    repeatTest(250, newGenerator(250, 50, 500, (25, 25, 30, 30)))
  }

  test("Large") {
    repeatTest(1, newGenerator(100, 20, 5000, (25, 35, 20, 20)))
  }


  def repeatTest(times: Int, generator: ProgramGenerator): Unit = {
    for (i <- 0 until times) {
      val program = generator.generate()
      val query = program.getRandomVar

      val (exhaustiveSolution, demandedSolution) = solveBoth(program, query)
      if (!Util.assertSolutions(exhaustiveSolution, demandedSolution, query)) {
        throw Error("Solutions did not match with query %d for program\n%s".format(query, PrettyPrinter.printProgram(program)))
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


}
