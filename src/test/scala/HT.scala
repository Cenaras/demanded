import Util.{assertEmpty, assertTokenIds, getBaseCvar, getFieldCvar}
import main.constraint.ConstraintGenerator
import main.program.*
import main.solver.HTSolver
import org.scalatest.funsuite.AnyFunSuite

class HT extends AnyFunSuite {

  test("LoadStore") {
    val p: Program = ProgramTemplates.LoadStore
    val solver = HTSolver();
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, 1)

    val x1 = getBaseCvar(solution, 1)
    assertTokenIds(x1, Seq(1, 2))
  }

  test("Aliasing") {
    val p: Program = ProgramTemplates.Aliasing
    val solver = HTSolver();
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, 5);
    assertTokenIds(getBaseCvar(solution, 5), Seq(2, 3))
  }

  test("Demanded") {
    val p: Program = ProgramTemplates.demandedSimple

    val solver = HTSolver();
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, 1)

    // x1 holds t1 and t2 by demanded. x3 should be empty as it is not required to compute x1
    assertTokenIds(getBaseCvar(solution, 1), Seq(1, 2))
    assert(getBaseCvar(solution, 3).solution.isEmpty)

  }

  test("Transitive Token Tracking") {
    val p: Program = ProgramTemplates.TransitiveTokenTracking

    val solver = HTSolver();
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, 3)
    assertTokenIds(getBaseCvar(solution, 3), Seq(1, 2))
  }

  test("Multiple Fields") {
    val p = ProgramTemplates.MultipleFields
    val solver = HTSolver()
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, (1, "f"))

    val x1 = getBaseCvar(solution, 1)
    val t1f = getFieldCvar(solution, 1, "f")
    val t2g = getFieldCvar(solution, 2, "g")
    val t3f = getFieldCvar(solution, 3, "f")
    val t3g = getFieldCvar(solution, 3, "g")

    assert(assertTokenIds(x1, Seq(1)))
    assert(assertTokenIds(t1f, Seq(2)))
    assert(assertTokenIds(t2g, Seq(1)))
    assert(assertEmpty(t3f))
    assert(assertEmpty(t3g))
  }
}
