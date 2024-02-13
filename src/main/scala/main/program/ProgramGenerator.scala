package main.program

import scala.util.Random

type ProgramDistribution = (Int, Int, Int, Int)

/**
 * A simple parser that parses programs programs for our simple language.
 */
object Parser {
  def ParseProgram(program: String): Program = {

    val newPattern = """x([0-9]+) = new t([0-9]+)""".r
    val assignPattern = """x([0-9]+) = x([0-9]+)""".r
    val loadPattern = """x([0-9]+) = x([0-9]+).([a-z])""".r
    val storePattern = """x([0-9]+).([a-z]) = x([0-9]+)""".r

    val p: Program = Program()

    val lines = program.split("\n")
    lines.foreach {
      case newPattern(x, t) => p.addInstruction(NewInsn(x.toInt, t.toInt))
      case assignPattern(left, right) => p.addInstruction(AssignInsn(left.toInt, right.toInt))
      case loadPattern(left, right, field) => p.addInstruction(LoadInsn(left.toInt, right.toInt, field))
      case storePattern(left, field, right) => p.addInstruction(StoreInsn(left.toInt, field, right.toInt))
      case e => throw Error("Error in parsing statement %s".format(e))
    }
    p
  }
}


/**
 * Generates random programs. Seeded with number of variables and number of instructions in the program.
 *
 * @param varNumber  number of variables.
 * @param tokenNum   number of tokens
 * @param insnNumber number of instructions.
 * @param dist       distribution of (new, assign, load, store) instructions respectively
 */
class ProgramGenerator(var varNumber: Int, var tokenNum: Int, var insnNumber: Int, dist: ProgramDistribution) {

  private val rng = new Random();
  private val fields = Array("f", "g")

  def generate(): Program = {
    val program: Program = Program()

    for (i <- 0 until insnNumber) {
      val number = rng.between(0, 100)
      program.addInstruction(genInstruction(number))
    }
    program
  }

  /**
   * Generate a random instruction based on the seed
   *
   * @param seed random number deciding the instruction to generate based on the distribution
   * @return
   */
  private def genInstruction(seed: Int): Instruction = {
    seed match {
      case x if x <= dist._1 =>
        val varId = generateRandomVar(None)
        val tokenId = generateRandomToken()
        NewInsn(varId, tokenId)
      case x if x <= dist._1 + dist._2 =>
        val leftId = generateRandomVar(None)
        val rightId = generateRandomVar(Some(leftId))
        AssignInsn(leftId, rightId)

      case x if x <= dist._1 + dist._2 + dist._3 =>
        val leftId = generateRandomVar(None)
        val rightId = generateRandomVar(Some(leftId))
        val field = generateRandomField()
        LoadInsn(leftId, rightId, field)
      case _ =>
        val leftId = generateRandomVar(None)
        val field = generateRandomField()
        val rightId = generateRandomVar(Some(leftId))
        StoreInsn(leftId, field, rightId)
    }
  }


  /**
   * Generates a random variable. If exclude is defined, the variable is guaranteed to be different from exclude
   *
   * @param exclude if specified, generated variable is guaranteed different from this.
   * @return random variable
   */
  private def generateRandomVar(exclude: Option[VarId]): Int = {
    val res = exclude match
      case None => rng.between(0, varNumber)
      case Some(ex) =>
        var random = -1
        while
          random = rng.between(0, varNumber)
          random == ex
        do ()
        random
    res
  }

  private def generateRandomToken(): Int = {
    rng.between(0, tokenNum)
  }

  private def generateRandomField(): String = {
    fields(rng.between(0, fields.length - 1))
  }
}


type VarId = Int
type TokenId = Int

sealed trait Instruction {
  def print(): String
}

case class NewInsn(varId: VarId, tokenId: TokenId) extends Instruction:
  override def print(): String = "x%d = new t%d".format(varId, tokenId)

case class AssignInsn(left: VarId, right: VarId) extends Instruction:
  override def print(): String = "x%d = x%d".format(left, right)

case class LoadInsn(left: VarId, right: VarId, field: String) extends Instruction:
  override def print(): String = "x%d = x%d.%s".format(left, right, field)

case class StoreInsn(left: VarId, field: String, right: VarId) extends Instruction:
  override def print(): String = "x%d.%s = x%d".format(left, field, right)
