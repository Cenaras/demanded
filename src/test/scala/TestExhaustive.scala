import main.constraint.{ConstraintGenerator, ConstraintVariables, FunToken}
import main.program.{Program, ProgramTemplates}
import main.solver.ExhaustiveSolver
import main.solver.SolverUtil.{containsExactly, getBaseCvar, getFieldCvar}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite


class TestExhaustive extends AnyFunSuite with BeforeAndAfterEach {

  test("LoadStore") {
    val solution = solve(ProgramTemplates.LoadStore)

    val x1 = getBaseCvar(solution, 1)
    val x2 = getBaseCvar(solution, 2)
    val x1f = getFieldCvar(solution, 1, "f")

    assert(x1.solution.size == 2)
    assert(x2.solution.size == 1)
    assert(x1f.solution.size == 1)

    assert(containsExactly(x1, Seq(1, 2)))
    assert(containsExactly(x2, Seq(2)))
    assert(containsExactly(x1f, Seq(2)))
  }

  test("Aliasing") {
    val solution = solve(ProgramTemplates.Aliasing)

    assert(containsExactly(solution, 1, Seq(1)))
    assert(containsExactly(solution, 1, "f", Seq(2, 3)))
    assert(containsExactly(solution, 2, Seq(2)))
    assert(containsExactly(solution, 3, Seq(1)))
    assert(containsExactly(solution, 4, Seq(3)))
    assert(containsExactly(solution, 5, Seq(2, 3)))
  }

  test("Multiple Fields") {
    val solution = solve(ProgramTemplates.MultipleFields)

    assert(containsExactly(solution, 1, Seq(1)))
    assert(containsExactly(solution, 1, "f", Seq(2)))
    assert(containsExactly(solution, 2, "g", Seq(1)))
    assert(containsExactly(solution, 3, "f", Seq(4)))
    assert(containsExactly(solution, 3, "g", Seq(4)))
  }

  test("FunCall") {
    val solution = solve(ProgramTemplates.FunCall)

    assert(containsExactly(solution, 6, Seq(2)))
    assert(containsExactly(solution, 6, Seq(2)))
    assert(containsExactly(solution, 8, Seq(3)))
    assert(containsExactly(solution, 1, Seq(1)))

    getBaseCvar(solution, 3).solution.foreach {
      case FunToken(id) =>
      case _ => throw Error("Expected only function tokens")
    }
  }

  test("FunctionAndField") {
    val solution = solve(ProgramTemplates.FunctionAndField)
    assert(containsExactly(solution, 4, Seq(1, 4)))
  }


  def solve(p: Program): ConstraintVariables = {
    val solver = ExhaustiveSolver()
    val constraints = ConstraintGenerator.generate(p)
    solver.solve(constraints)
  }

}


