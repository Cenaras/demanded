import main.constraint.ConstraintGenerator
import main.program.{ProgramDistribution, ProgramGenerator}
import main.solver.{ExhaustiveSolver, HTSolver}
import main.util.PrettyPrinter
import org.scalatest.Distributor
import org.scalatest.funsuite.AnyFunSuite


class Compare extends AnyFunSuite {


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

      val constraints = ConstraintGenerator.generate(program)

      val exhaustive = ExhaustiveSolver()
      val ht = new HTSolver()

      val exhaustiveSolution = exhaustive.solve(constraints)
      val demandedSolution = ht.solve(constraints, query)
      if (!Util.assertSolutions(exhaustiveSolution, demandedSolution, query)) {
        throw Error("Solutions did not match for program\n%s".format(PrettyPrinter.printProgram(program)))
      }
      
      Util.assertSizeOfPointsToFacts(exhaustiveSolution, demandedSolution)


    }
  }

  def newGenerator(varNumber: Int, tokenNum: Int, size: Int, dist: ProgramDistribution): ProgramGenerator = {
    new ProgramGenerator(varNumber, tokenNum, size, dist)
  }


}
