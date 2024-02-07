import scala.collection.mutable.ArrayBuffer
import scala.util.Random

type Program = ArrayBuffer[Instruction];

/**
 * Generates random programs. Seeded with number of variables and number of instructions in the program.
 * @param varNumber number of variables.
 * @param insnNumber number of instructions.
 */
class ProgramGenerator(var varNumber: Int, var tokenNum: Int, var insnNumber: Int) {

  // Probability of new, assign, load, store respectively
  private val opProp = (20, 50, 20, 10)
  private val rng = new Random();

  def generate(): Program = {
    println("Producing random program with %d variables, %d tokens and %d instructions".format(varNumber, tokenNum, insnNumber))

    val program: Program = ArrayBuffer();

    for (i <- 0 until insnNumber) {
      val number = rng.between(0, 100)
      program += genInsn(number)
    }
    program
  }

  private def genInsn(seed: Int): Instruction = {
    seed match {
      case x if x <= opProp._1 =>
        val varId = randomVar(None)
        val tokenId = randomToken()
        NewInsn(varId, tokenId)
      case x if x <= opProp._1 + opProp._2 =>
        val leftId = randomVar(None)
        val rightId = randomVar(Some(leftId))
        AssignInsn(leftId, rightId)

      case x if x <= opProp._1 + opProp._2 + opProp._3 =>
        val leftId = randomVar(None)
        val rightId = randomVar(Some(leftId))
        val field = "f"
        LoadInsn(leftId, rightId, field)
      case _ =>
        val leftId = randomVar(None)
        val field = "f"
        val rightId = randomVar(Some(leftId))
        StoreInsn(leftId, field, rightId)
    }
  }


  /**
   * Generates a random variable. If exclude is defined, the variable is guaranteed to be different from exclude
   * @param exclude if specified, generated variable is guaranteed different from this.
   * @return random variable
   */
  private def randomVar(exclude: Option[VarId]): Int = {
   var res = exclude match
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

  private def randomToken(): Int = {
    rng.between(0, tokenNum)
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

//case class Variable(id: Int):
//   def print(): String = "x%d".format(id)
//   def equals(other: Variable): Boolean = id == other.id