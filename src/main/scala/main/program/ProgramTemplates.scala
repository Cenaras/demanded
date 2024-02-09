package main.program

import scala.collection.mutable.ArrayBuffer

object ProgramTemplates {

  /**
   *
   * x1 = new t1
   * x2 = new t2
   * x1 = x2
   * x3 = new t2
   * x1.f = x3
   * x4 = x1.f
   */
  def LoadStore: Program = {
    Program(
      ArrayBuffer[Instruction](
        NewInsn(1, 1),
        NewInsn(2, 2),
        AssignInsn(1, 2),
        NewInsn(3, 2),
        StoreInsn(1, "f", 3),
        LoadInsn(4, 1, "f")
      ))
  }

  /**
   * // x = {f: {}}
   * x1 = new t1
   * x2 = new t2
   * x1.f = x2
   *
   * // z = x
   * x3 = x1
   *
   * // z.f = {}
   * x4 = new t3
   * x3.f = x4
   *
   * // y = x.f
   * x5 = x1.f
   */
  def Aliasing: Program = {
    Program(
      ArrayBuffer[Instruction](
        NewInsn(1, 1),
        NewInsn(2, 2),
        StoreInsn(1, "f", 2),
        AssignInsn(3, 1),
        NewInsn(4, 3),
        StoreInsn(3, "f", 4),
        LoadInsn(5, 1, "f")
      ))
  }

  /**
   * x1 = new t1
   * x2 = new t2
   * x3 = new t3
   * x1 = x2
   */
  def demandedSimple: Program = {
    Program(
      ArrayBuffer[Instruction](
        NewInsn(1, 1),
        NewInsn(2, 2),
        NewInsn(3, 3),
        AssignInsn(1, 2)
      ))
  }

  /**
   * This program previously gave us problems. Since we are reading from x3.f, we must track all tokens from x3, which
   * in this example initially will be t1. Then we must propagate t1 over to x4 and then from there to x2, such that
   * x2.f will actually perform the write (placing x1 in demand, then x2, then x0 which leads us to the t0 token)
   * This was implemented wrongly previously which caused an issue.
   *
   * x0 = new t0
   * x4 = new t1
   *
   * x0 = x4         x0 holds t0 t1
   * x2 = x0         x2 holds t0 t1
   * x1 = x2         x1 holds t0 t1
   *
   * x2.f = x1       t0.f and t1.f hold t0 and t1
   *
   * x3 = x4         x3 holds t1
   * x4 = x3.f       x4 holds t0, t1  which means x3 holds both due to above
   *
   */
  def TransitiveTokenTracking: Program = {
    Program(
      ArrayBuffer[Instruction](
        NewInsn(0, 0),
        NewInsn(4, 1),
        AssignInsn(0, 4),
        AssignInsn(2, 0),
        AssignInsn(1, 2),
        StoreInsn(2, "f", 1),
        AssignInsn(3, 4),
        LoadInsn(4, 3, "f"),
      )
    )
  }

  /**
   * This small program gave us problems with query id 4, due to tracked tokens not being propagated unconditionally in
   * a store operation. An error in our translation made it such that tracked tokens were only propagated if demand was
   * placed on the receiver constraint variable, which was wrong, as tracked tokens must always be propagated.
   *
   * x4 = new t1
   * x5 = new t2
   *
   * x4 = x3
   * x3 = x4.f
   *
   * x5.f = x4
   * x1.f = x5
   * x1 = x5.f
   */
  def UnconditionalTokenTrackingInStore: Program = {
    Program(
      ArrayBuffer[Instruction](
        NewInsn(4, 1),
        NewInsn(5, 2),
        AssignInsn(4, 3),
        StoreInsn(5, "f", 4),
        StoreInsn(1, "f", 5),
        LoadInsn(3, 4, "f"),
        LoadInsn(1, 5, "f"),
      )
    )
  }


  /* query 6
x1 = new t1
x5 = new t1
x3 = new t2
x0.f = x1
x6 = x3.f
x0 = x5.f
x1.f = x3
*/

  /*
    We initially place x6 in demand
    From x6 = x3.f we place x3 in demand and thus t2 in x3
      We track t2 and demand t2.f
    From x1.f = x3 we demand x1
      Since x3 holds t2 and t2 is tracked
      t1 in x1 now
      t2 in t1.f now


    TODO: We are missing the flow of t2 into x0.
      We are adding the entire x to demanded, but we were never tracking the tokens meaning alias relations
      for x1 and x5 were missed, so we never processed x0 = x5.f and thereby t2 never flowed into x0


  Exhaustive solution
  t1 in x1, t1 in x5, t2 in x3
  From x1.f = x3, we get that t1.f has t2
  From x0 = x5.f we get that x0 has t2 (due to above placing t2 in t1.f and t1 in x5)
  From x0.f = x1 we get t2.f has t1
  From x6 = x3.f we get x6 has t1


  Demand:
    x6 (query)
    x3 (x6 = x3.f)
    t2.f (x6 = x3.f since t2 in x3)
    x1 (x1.f = x3 since t2 is tracked)

  Tracked:
    t2 (x6 = x3.f since t2 in x3)


  */


  def XXX: Program = {
    val program = "x1 = new t1\nx5 = new t1\nx3 = new t2\nx0.f = x1\nx6 = x3.f\nx0 = x5.f\nx1.f = x3"
    Parser.ParseProgram(program)
  }


  /**
   * Reads a template from the templates folder
   *
   * @param name filename from template folder.
   * @return contents of filename
   */
  private def readTemplate(name: String): String = {
    val source = scala.io.Source.fromFile("src/main/scala/main/program/templates/" + name)
    val lines = try source.mkString finally source.close()
    lines //TODO: This does not work it seems, whatever is read maybe contains \cr or something so we dont match
  }


}
