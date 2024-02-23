import main.program.{ProgramDistribution, ProgramGenerator}
import main.solver.{HTSolver, LMSolver}
import org.scalatest.funsuite.AnyFunSuite

class Diagnostics extends AnyFunSuite {


  test("HT vs LM small") {
    for (i <- 0 until 10000) {
      val g = ProgramGenerator(25, 5, 55, ProgramDistribution(20, 40, 20, 20))
      val p = g.generate()
      val query = p.getRandomVar

      val (diagHT, diagLM) = TestUtil.compareOptimality(p, query, HTSolver(), LMSolver())
      assert(diagHT == diagLM)
    }
  }
}
