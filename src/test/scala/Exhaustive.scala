import Util.{assertTokenIds, getBaseCvar, getFieldCvar}
import main.constraint.ConstraintGenerator
import main.program.{Program, ProgramTemplates}
import main.solver.ExhaustiveSolver
import org.scalatest.funsuite.AnyFunSuite


class Exhaustive extends AnyFunSuite {

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

    assert(assertTokenIds(x1, Seq(1, 2)))
    assert(assertTokenIds(x2, Seq(2)))
    assert(assertTokenIds(x1f, Seq(2)))
  }

  test("Aliasing") {

    val p: Program = ProgramTemplates.Aliasing

    val solver = ExhaustiveSolver()
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints)


    val x1 = getBaseCvar(solution, 1)
    val x2 = getBaseCvar(solution, 2)
    val x3 = getBaseCvar(solution, 3)
    val x4 = getBaseCvar(solution, 4)
    val x5 = getBaseCvar(solution, 5)
    val t1f = getFieldCvar(solution, 1, "f")
    assert(assertTokenIds(x1, Seq(1)))
    assert(assertTokenIds(t1f, Seq(2, 3)))
    assert(assertTokenIds(x2, Seq(2)))
    assert(assertTokenIds(x3, Seq(1)))
    assert(assertTokenIds(x4, Seq(3)))
    assert(assertTokenIds(x5, Seq(2, 3)))
  }

  test("Multiple Fields") {
    val p = ProgramTemplates.MultipleFields
    val solver = ExhaustiveSolver()
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints)

    val x1 = getBaseCvar(solution, 1)
    val t1f = getFieldCvar(solution, 1, "f")
    val t2g = getFieldCvar(solution, 2, "g")
    val t3f = getFieldCvar(solution, 3, "f")
    val t3g = getFieldCvar(solution, 3, "g")

    assert(assertTokenIds(x1, Seq(1)))
    assert(assertTokenIds(t1f, Seq(2)))
    assert(assertTokenIds(t2g, Seq(1)))
    assert(assertTokenIds(t3f, Seq(4)))
    assert(assertTokenIds(t3g, Seq(4)))
  }
}


