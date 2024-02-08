import Util.{assertTokenIds, getCvar}
import main.program.{AssignInsn, Instruction, LoadInsn, NewInsn, Program, ProgramTemplates, StoreInsn}
import main.solver.HTSolver
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

class HT extends AnyFunSuite {


  test("LoadStore") {
    val p: Program = ProgramTemplates.LoadStore

    val solver = HTSolver(p);
    val constraints = solver.generateConstraints()
    val solution = solver.solve(constraints, 1)

    val x1 = getCvar(solution, 1, true)
    assertTokenIds(x1, Seq(1, 2))
  }


  test("Aliasing") {



    val p: Program = ProgramTemplates.Aliasing 

    val solver = HTSolver(p);
    val constraints = solver.generateConstraints()
    val solution = solver.solve(constraints, 5);
    assertTokenIds(getCvar(solution, 5, true), Seq(2, 3))


  }

  test("Demanded") {
    /*
      x1 = new t1
      x2 = new t2
      x3 = new t3
      x1 = x2
    */

    val p: Program = Program(
      ArrayBuffer[Instruction](
        NewInsn(1, 1),
        NewInsn(2, 2),
        NewInsn(3, 3),
        AssignInsn(1, 2)
      ))

    val solver = HTSolver(p);
    val constraints = solver.generateConstraints()
    val solution = solver.solve(constraints, 1)

    // x1 holds t1 and t2 by demanded. x3 should be empty as it is not required to compute x1
    assertTokenIds(getCvar(solution, 1, true), Seq(1, 2))
    assert(getCvar(solution, 3, true).solution.isEmpty)

  }


}
