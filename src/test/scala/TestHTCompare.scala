import TestUtil.*
import main.program.ProgramTemplates
import main.solver.SolverUtil.*
import main.solver.{HTDouble, HTSolver}
import main.util.PrettyPrinter
import org.scalatest.funsuite.AnyFunSuite

class TestHTCompare extends AnyFunSuite {


  test("Compare") {
    repeatSolveBoth(10000, newGenerator(20, 8, 60, newDist(20, 40, 20, 20)), solveBothDemanded, HTSolver(), HTDouble())
    repeatSolveBoth(10000, newGenerator(20, 8, 60, newDist(20, 40, 20, 20)), solveBothDemanded, HTDouble(), HTSolver())
  }

  test("Multiple Fields") {
    val p = ProgramTemplates.MultipleFields
    val (one, two) = solveBothDemanded(p, 1, HTSolver(), HTDouble())
    assert(compareSolutions(one, two, 1))

    var sol = solveBothDemanded(p, (3, "f"), HTSolver(), HTDouble())
    assert(compareSolutions(sol._1, sol._2, (3, "f")))

    sol = solveBothDemanded(p, (3, "g"), HTSolver(), HTDouble())
    assert(compareSolutions(sol._1, sol._2, (3, "g")))

  }


}
