import main.constraint.ConstraintVar
import main.program.{AssignInsn, Instruction, LoadInsn, NewInsn, Program, StoreInsn}
import main.solver.{ConstraintVariables, ExhaustiveSolver}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

class Exhaustive extends AnyFunSuite {

  test("LoadStore") {

    /*
      x1 = new t1
      x2 = new t2
      x1 = x2
      x3 = new t2
      x1.f = x3
      x4 = x1.f
     */
    val p: Program = ArrayBuffer[Instruction](
      NewInsn(1, 1),
      NewInsn(2, 2),
      AssignInsn(1, 2),
      NewInsn(3, 2),
      StoreInsn(1, "f", 3),
      LoadInsn(4, 1, "f")
    )

    val solver = ExhaustiveSolver(p);
    solver.generateConstraints()
    val solution = solver.solve();


    val x1 = getCvar(solution, 1, true)
    val x2 = getCvar(solution, 2, true)
    val x1f = getCvar(solution, 1, false)

    assert(x1.solution.size == 2)
    assert(x2.solution.size == 1)
    assert(x1f.solution.size == 1)

    assertTokenIds(x1, Seq(1, 2))
    assertTokenIds(x2, Seq(2))
    assertTokenIds(x1f, Seq(2))
  }

  test("Aliasing") {
    /*
      // x = {f: {}}
      x1 = new t1
      x2 = new t2
      x1.f = x2

      // z = x
      x3 = x1

      // z.f = {}
      x4 = new t3
      x3.f = x4

      // y = x.f
      x5 = x1.f
     */
    val p: Program = ArrayBuffer[Instruction](
      NewInsn(1, 1),
      NewInsn(2, 2),
      StoreInsn(1, "f", 2),
      AssignInsn(3, 1),
      NewInsn(4, 3),
      StoreInsn(3, "f", 4),
      LoadInsn(5, 1, "f")
    )

    val solver = ExhaustiveSolver(p);
    solver.generateConstraints()
    val solution = solver.solve();


    val x1 = getCvar(solution, 1, true)
    val x2 = getCvar(solution, 2, true)
    val x3 = getCvar(solution, 3, true)
    val x4 = getCvar(solution, 4, true)
    val x5 = getCvar(solution, 5, true)
    val t1f = getCvar(solution, 1, false)
    assertTokenIds(x1, Seq(1))
    assertTokenIds(t1f, Seq(2, 3))
    assertTokenIds(x2, Seq(2))
    assertTokenIds(x3, Seq(1))
    assertTokenIds(x4, Seq(3))
    assertTokenIds(x5, Seq(2, 3))
  }













  def getCvar(solution: ConstraintVariables, id: Int, base: Boolean): ConstraintVar = {
    solution.find(c => c.getId == id && base == c.base).get;
  }

  def assertTokenIds(cvar: ConstraintVar, expectedIds: Seq[Int]): Unit = {
    var result: Boolean = true;

    val tokenIds: Seq[Int] = cvar.solution.map(f => f.id).toSeq

    for (t <- expectedIds) {
      result &= tokenIds.contains(t)
    }

    assert(result)

  }


}
