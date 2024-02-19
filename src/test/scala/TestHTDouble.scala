import TestUtil.solve
import main.program.ProgramTemplates
import main.solver.HTDouble
import org.scalatest.funsuite.AnyFunSuite

class TestHTDouble extends AnyFunSuite {

  test("LoadStore") {
    val solution = solve(ProgramTemplates.LoadStore, 1, HTDouble())

  }

}
