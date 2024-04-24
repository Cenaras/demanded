type Var = Int
type Token = Int
type Field = Int

class Program(instructions: List[Instruction]) {

  def this() = {
    this(List())
  }

  def print(): Unit =
    instructions.foreach(println)

  def getInstructions: List[Instruction] = instructions

}

sealed trait Instruction:
  override def toString: String

case class New(x: Var, t: Token) extends Instruction:
  override def toString: String = s"x$x = new t$t"

case class Assign(x: Var, y: Var) extends Instruction:
  override def toString: String = s"x$x = x$y"

case class Load(x: Var, y: Var, f: Field) extends Instruction:
  override def toString: String = s"x$x = x$y.f$f"

case class Store(x: Var, f: Field, y: Var) extends Instruction:
  override def toString: String = s"x$x.f$f = x$y"
