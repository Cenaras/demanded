import Util.{assertTokens, getBaseCvar, getFieldCvar}
import main.constraint.ConstraintGenerator
import main.program.{Program, ProgramTemplates}
import main.solver.ExhaustiveSolver
import org.scalatest.funsuite.AnyFunSuite


class TestExhaustive extends AnyFunSuite {

  test("LoadStore") {
    val p: Program = ProgramTemplates.LoadStore

    val solver = ExhaustiveSolver()
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints)

    val x1 = getBaseCvar(solution, 1)
    val x2 = getBaseCvar(solution, 2)
    val x1f = getFieldCvar(solution, 1, "f")

    assert(x1.solution.size == 2)
    assert(x2.solution.size == 1)
    assert(x1f.solution.size == 1)

    assert(assertTokens(x1, Seq(1, 2)))
    assert(assertTokens(x2, Seq(2)))
    assert(assertTokens(x1f, Seq(2)))
  }

  test("Aliasing") {

    val p: Program = ProgramTemplates.Aliasing

    val solver = ExhaustiveSolver()
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints)

    assert(assertTokens(solution, 1, Seq(1)))
    assert(assertTokens(solution, 1, "f", Seq(2, 3)))
    assert(assertTokens(solution, 2, Seq(2)))
    assert(assertTokens(solution, 3, Seq(1)))
    assert(assertTokens(solution, 4, Seq(3)))
    assert(assertTokens(solution, 5, Seq(2, 3)))
  }

  test("Multiple Fields") {
    val p = ProgramTemplates.MultipleFields
    val solver = ExhaustiveSolver()
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints)

    assert(assertTokens(solution, 1, Seq(1)))
    assert(assertTokens(solution, 1, "f", Seq(2)))
    assert(assertTokens(solution, 2, "g", Seq(1)))
    assert(assertTokens(solution, 3, "f", Seq(4)))
    assert(assertTokens(solution, 3, "g", Seq(4)))
  }
}


