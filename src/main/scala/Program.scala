type Var = Int
type Token = Int
type Field = Int

// TODO: We could clean this up into a single class and use polymorphisms if we wanted to

class Program(instructions: List[Instruction]) {

  def this() = {
    this(List())
  }

  def print(): Unit =
    instructions.foreach(println)

  def getInstructions: List[Instruction] = instructions

}

class CProgram(instructions: List[CInstruction]) {
  def this() = {
    this(List())
  }

  def print(): Unit =
    instructions.foreach(println)

  def getInstructions: List[CInstruction] = instructions
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


sealed trait CInstruction:
  override def toString: String

case class AddrOf(x: Var, y: Var) extends CInstruction:
  override def toString: String = s"x$x = &x$y"

case class Copy(x: Var, y: Var) extends CInstruction:
  override def toString: String = s"x$x = x$y"
  
// TODO: Field sensitive
case class CLoad(x: Var, y: Var) extends CInstruction:
  override def toString: String = s"x$x = *x$y"
  
case class CStore(x: Var, y: Var) extends CInstruction:
  override def toString: String = s"*x$x = x$y"

// Instruction representing LLVM GetElementPtr - computes memory address starting from base and following offset
// TODO: Solving should say: ∀t ∈ ⟦base⟧ : t + offset ∈ ⟦dst⟧
case class Gep(dst: Var, base: Var, offset: Int) extends CInstruction:
  override def toString: String = s"x$dst = gep($base, $offset)"