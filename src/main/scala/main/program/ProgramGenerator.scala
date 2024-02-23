package main.program

import java.io.FileWriter
import scala.util.Random

/**
 * A simple parser that parses programs programs for our simple language.
 */
object Parser {
  def ParseProgram(program: String): Program = {

    val newPattern = """x([0-9]+) = new t([0-9]+)""".r
    val assignPattern = """x([0-9]+) = x([0-9]+)""".r
    val loadPattern = """x([0-9]+) = x([0-9]+).([a-z])""".r
    val storePattern = """x([0-9]+).([a-z]) = x([0-9]+)""".r
    val newFunPattern = """x([0-9]+) = \(x([0-9]+)\) =>_f([0-9]+) x([0-9]+)""".r //FIXME: Use capture group to ensure always identity func
    val callPattern = """x([0-9]+) = x([0-9]+)\(x([0-9]+)\)""".r


    val p: Program = Program()

    val lines = program.split("\n")
    lines.foreach {
      case newPattern(x, t) => p.addInstruction(NewInsn(x.toInt, t.toInt))
      case assignPattern(left, right) => p.addInstruction(AssignInsn(left.toInt, right.toInt))
      case loadPattern(left, right, field) => p.addInstruction(LoadInsn(left.toInt, right.toInt, field))
      case storePattern(left, field, right) => p.addInstruction(StoreInsn(left.toInt, field, right.toInt))
      case newFunPattern(left, arg, funId, retVal) => p.addInstruction(NewFunInsn(left.toInt, arg.toInt, retVal.toInt, funId.toInt))
      case callPattern(res, cls, arg) => p.addInstruction(CallInsn(res.toInt, cls.toInt, arg.toInt))
      case e => throw Error("Error in parsing statement %s".format(e))
    }
    p
  }

  /**
   * Writes the given program into the facts files in the src/datalog directory
   *
   * @param p the program to translate
   */
  def WriteDatalog(p: Program): Unit = {
    var newFacts = ""
    var assignFacts = ""
    var loadFacts = ""
    var storeFacts = ""

    p.getInstructions.foreach {
      case NewInsn(varId, tokenId) => newFacts += "x%s\tt%s\n".format(varId, tokenId)
      case AssignInsn(left, right) => assignFacts += "x%s\tx%s\n".format(left, right)
      case LoadInsn(left, right, field) => loadFacts += "x%s\tx%s\t%s\n".format(left, right, field)
      case StoreInsn(left, field, right) => storeFacts += "x%s\t%s\tx%s\n".format(left, field, right)
      case _ => throw Error("Unsupported datalog translation")
    }

    def write(file: String, content: String): Unit = {
      val fw = new FileWriter("src/datalog/" + file + ".facts")
      fw.write(content)
      fw.close()
    }

    write("new", newFacts)
    write("assign", assignFacts)
    write("load", loadFacts)
    write("store", storeFacts)

  }


}

class ProgramDistribution(newObj: Int, assign: Int, load: Int, store: Int, newFun: Int, call: Int) {

  def this(newObj: Int, assign: Int, load: Int, store: Int) = {
    this(newObj, assign, load, store, 0, 0)
  }

  assert(callProp == 100)

  def newObjProp: Int = newObj

  def assignProp: Int = newObjProp + assign

  def loadProp: Int = assignProp + load

  def storeProp: Int = loadProp + store

  def newFunProp: Int = storeProp + newFun

  def callProp: Int = newFunProp + call
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
  private var currentToken = 0


  def generate(): Program = {
    val program: Program = Program()
    currentToken = 0

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
      case x if x <= dist.newObjProp =>
        val varId = generateRandomVar(None)
        val tokenId = generateRandomToken()
        NewInsn(varId, tokenId)
      case x if x <= dist.assignProp =>
        val leftId = generateRandomVar(None)
        val rightId = generateRandomVar(leftId)
        AssignInsn(leftId, rightId)
      case x if x <= dist.loadProp =>
        val leftId = generateRandomVar(None)
        val rightId = generateRandomVar(leftId)
        val field = generateRandomField()
        LoadInsn(leftId, rightId, field)
      case x if x <= dist.storeProp =>
        val leftId = generateRandomVar(None)
        val field = generateRandomField()
        val rightId = generateRandomVar(leftId)
        StoreInsn(leftId, field, rightId)
      case x if x <= dist.newFunProp =>
        val funId = generateRandomVar(None)
        val funToken = generateRandomToken()
        val param = generateRandomParameter()
        val retVal = generateRandomVar(funId)
        NewFunInsn(funId, param, retVal, funToken)
      case x if x <= dist.callProp =>
        val resNode = generateRandomVar(None)
        val callNode = generateRandomVar(resNode)
        val argNode = generateRandomVar(Some(Seq(resNode, callNode)))
        CallInsn(resNode, callNode, argNode)
    }
  }


  /**
   * Generates a random variable. If exclude is defined, the variable is guaranteed to be different from exclude
   *
   * @param exclude if specified, generated variable is guaranteed different from this.
   * @return random variable
   */
  private def generateRandomVar(exclude: Option[Seq[VarId]]): Int = {
    val res = exclude match
      case None => rng.between(0, varNumber)
      case Some(ex) =>
        var random = -1
        while
          random = rng.between(0, varNumber)
          ex.contains(random)
        do ()
        random
    res
  }

  private def generateRandomVar(exclude: VarId): Int = {
    generateRandomVar(Some(Seq(exclude)))
  }

  private def generateRandomToken(): Int = {
    val t = currentToken
    currentToken += 1
    t
  }

  private def generateRandomField(): String = {
    fields(rng.between(0, fields.length - 1))
  }

  // FIXME: Generate in a better range, so functions can actually return their arguments...
  private def generateRandomParameter(): Int = {
    rng.between(0, varNumber) + varNumber
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

// Functions take a random argument and return a random variable.
case class NewFunInsn(varId: VarId, argId: VarId, retVal: VarId, tokenId: TokenId) extends Instruction:
  override def print(): String = "x%d = (x%d) =>_f%d x%d".format(varId, argId, tokenId, retVal)

case class CallInsn(res: VarId, fun: VarId, arg: VarId) extends Instruction:
  override def print(): String = "x%d = x%d(x%d)".format(res, fun, arg)
