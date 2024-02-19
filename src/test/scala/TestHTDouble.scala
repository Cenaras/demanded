import TestUtil.solve
import main.program.ProgramTemplates
import main.solver.HTDouble
import main.solver.SolverUtil.{containsExactly, getBaseCvar, getFieldCvar, isEmpty}
import org.scalatest.funsuite.AnyFunSuite

class TestHTDouble extends AnyFunSuite {

  test("LoadStore") {
    val solution = solve(ProgramTemplates.LoadStore, 1, HTDouble())
    val x1 = getBaseCvar(solution, 1)
    containsExactly(x1, Seq(1, 2))
  }

  test("Multiple Fields") {
    val solution = solve(ProgramTemplates.MultipleFields, (1, "f"), HTDouble())

    assert(containsExactly(solution, 1, Seq(1)))
    assert(containsExactly(solution, 1, "f", Seq(2)))
    assert(containsExactly(solution, 2, "g", Seq(1)))
    assert(isEmpty(getFieldCvar(solution, 3, "f")))
    assert(isEmpty(getFieldCvar(solution, 3, "g")))
  }
}
