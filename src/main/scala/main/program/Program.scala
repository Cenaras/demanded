package main.program

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Program(instructions: ArrayBuffer[Instruction]) {

  def this() = {
    this(ArrayBuffer())
  }

  private val varIds = mutable.Set[Int]()

  def addInstruction(instruction: Instruction): Unit = {
    instructions.addOne(instruction)
    instruction match
      case NewInsn(varId, tokenId) => varIds.add(varId)
      case AssignInsn(leftId, rightId) =>
        varIds.add(leftId)
        varIds.add(rightId)

      case LoadInsn(left, right, field) =>
        varIds.add(left)
        varIds.add(right)

      case StoreInsn(left, field, right) =>
        varIds.add(left)
        varIds.add(right)

      case NewFunInsn(varId, argId, tokenId) =>
        varIds.add(varId)

      case CallInsn(res, fun, arg) =>
        varIds.add(res)
        varIds.add(fun)
        varIds.add(arg)

  }


  def getInstructions: ArrayBuffer[Instruction] = instructions


  /**
   * Retrieves a random program variable, guaranteed to be present.
   *
   * @return random variable from the program
   */
  def getRandomVar: Int = {
    varIds.iterator.drop(Random.nextInt(varIds.size)).next()
  }


}
