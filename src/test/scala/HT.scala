import Util.{assertTokenIds, getCvar}
import main.constraint.ConstraintGenerator
import main.program.*
import main.solver.HTSolver
import main.util.PrettyPrinter
import org.scalatest.funsuite.AnyFunSuite

class HT extends AnyFunSuite {


  test("LoadStore") {
    val p: Program = ProgramTemplates.LoadStore

    val solver = HTSolver();
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, 1)

    val x1 = getCvar(solution, 1, true)
    assertTokenIds(x1, Seq(1, 2))
  }


  test("Aliasing") {
    val p: Program = ProgramTemplates.Aliasing

    val solver = HTSolver();
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, 5);
    assertTokenIds(getCvar(solution, 5, true), Seq(2, 3))

    println(PrettyPrinter.printSolution(solution))


  }

  test("Demanded") {
    val p: Program = ProgramTemplates.demandedSimple

    val solver = HTSolver();
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, 1)

    // x1 holds t1 and t2 by demanded. x3 should be empty as it is not required to compute x1
    assertTokenIds(getCvar(solution, 1, true), Seq(1, 2))
    assert(getCvar(solution, 3, true).solution.isEmpty)

  }

  test("Transitive Token Tracking") {
    val p: Program = ProgramTemplates.TransitiveTokenTracking

    val solver = HTSolver();
    val constraints = ConstraintGenerator.generate(p)
    val solution = solver.solve(constraints, 3)
    assertTokenIds(getCvar(solution, 3, true), Seq(1, 2))
  }
}
