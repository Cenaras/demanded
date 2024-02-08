package main.program

import scala.collection.mutable.ArrayBuffer

object ProgramTemplates {

  /**
   *
   x1 = new t1
   x2 = new t2
   x1 = x2
   x3 = new t2
   x1.f = x3
   x4 = x1.f
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


}
