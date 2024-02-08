import Util.{assertTokenIds, getCvar}
import main.program.{AssignInsn, Instruction, LoadInsn, NewInsn, Program, ProgramTemplates, StoreInsn}
import main.solver.{ConstraintVariables, ExhaustiveSolver}
import org.scalatest.funsuite.AnyFunSuite


class Exhaustive extends AnyFunSuite {

  test("LoadStore") {


    val p: Program = ProgramTemplates.LoadStore

    val solver = ExhaustiveSolver(p);
    val constraints = solver.generateConstraints()
    val solution = solver.solve(constraints);


    val x1 = getCvar(solution, 1, true)
    val x2 = getCvar(solution, 2, true)
    val x1f = getCvar(solution, 1, false)

    assert(x1.solution.size == 2)
    assert(x2.solution.size == 1)
    assert(x1f.solution.size == 1)

    assert(assertTokenIds(x1, Seq(1, 2)))
    assert(assertTokenIds(x2, Seq(2)))
    assert(assertTokenIds(x1f, Seq(2)))
  }

  test("Aliasing") {

    val p: Program = ProgramTemplates.Aliasing

    val solver = ExhaustiveSolver(p);
    val constraints = solver.generateConstraints()
    val solution = solver.solve(constraints);


    val x1 = getCvar(solution, 1, true)
    val x2 = getCvar(solution, 2, true)
    val x3 = getCvar(solution, 3, true)
    val x4 = getCvar(solution, 4, true)
    val x5 = getCvar(solution, 5, true)
    val t1f = getCvar(solution, 1, false)
    assert(assertTokenIds(x1, Seq(1)))
    assert(assertTokenIds(t1f, Seq(2, 3)))
    assert(assertTokenIds(x2, Seq(2)))
    assert(assertTokenIds(x3, Seq(1)))
    assert(assertTokenIds(x4, Seq(3)))
    assert(assertTokenIds(x5, Seq(2, 3)))
  }

}
