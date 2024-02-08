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
      
      
      val exhaustive = ExhaustiveSolver(program)
      val exhaustiveConstraints = exhaustive.generateConstraints()

      val ht = new HTSolver(program)
      val demandedConstraints = ht.generateConstraints()

      val exhaustiveSolution = exhaustive.solve(exhaustiveConstraints)
      val demandedSolution = ht.solve(demandedConstraints, query)
      if (!Util.assertSolutions(exhaustiveSolution, demandedSolution, query)) {
        throw Error("Solutions did not match for program\n%s".format(PrettyPrinter.printProgram(program)))
      }
    }


  }
}
