import main.constraint.ConstraintGenerator
import main.program.ProgramGenerator
import main.solver.{ExhaustiveSolver, HTSolver}
import main.util.PrettyPrinter
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class Compare extends AnyFunSuite {


  test("Single") {

  }

  test("Random program") {
    val varNumber = 100
    val tokenNum = 30
    val programSize = 200
    val generator = new ProgramGenerator(varNumber, tokenNum, programSize)

    val iterations = 100


    for (i <- 0 until iterations) {


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
    }


  }
}
