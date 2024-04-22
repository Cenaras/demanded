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

case class Assign(left: Var, right: Var) extends Instruction:
  override def toString: String = s"x$left = x$right"

case class Load(res: Var, base: Var, field: Field) extends Instruction:
  override def toString: String = s"x$res = x$base.f$field"

case class Store(base: Var, field: Field, value: Var) extends Instruction:
  override def toString: String = s"x$base.f$field = x$value"
