import Util.{assertEmpty, assertTokens, getBaseCvar, getFieldCvar}
import main.constraint.ConstraintGenerator
import main.program.*
import main.solver.HTSolver
import org.scalatest.funsuite.AnyFunSuite

class TestHTSolver extends AnyFunSuite {

  test("LoadStore") {
    val p: Program = ProgramTemplates.LoadStore
    val solver = HTSolver();
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, 1)

    val x1 = getBaseCvar(solution, 1)
    assertTokens(x1, Seq(1, 2))
  }

  test("Aliasing") {
    val p: Program = ProgramTemplates.Aliasing
    val solver = HTSolver();
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, 5);
    assertTokens(getBaseCvar(solution, 5), Seq(2, 3))
  }

  test("Demanded") {
    val p: Program = ProgramTemplates.demandedSimple

    val solver = HTSolver();
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, 1)

    // x1 holds t1 and t2 by demanded. x3 should be empty as it is not required to compute x1
    assertTokens(getBaseCvar(solution, 1), Seq(1, 2))
    assert(getBaseCvar(solution, 3).solution.isEmpty)

  }

  test("Transitive Token Tracking") {
    val p: Program = ProgramTemplates.TransitiveTokenTracking

    val solver = HTSolver();
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, 3)
    assertTokens(getBaseCvar(solution, 3), Seq(1, 2))
  }

  test("Multiple Fields") {
    val p = ProgramTemplates.MultipleFields
    val solver = HTSolver()
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, (1, "f"))

    assert(assertTokens(solution, 1, Seq(1)))
    assert(assertTokens(solution, 1, "f", Seq(2)))
    assert(assertTokens(solution, 2, "g", Seq(1)))
    assert(assertEmpty(getFieldCvar(solution, 3, "f")))
    assert(assertEmpty(getFieldCvar(solution, 3, "g")))
  }
}
