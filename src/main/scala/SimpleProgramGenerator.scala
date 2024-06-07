import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Distribution(val newObj: Int = 20, val assign: Int = 40, val load: Int = 20, val store: Int = 20) {

  assert(storeProp == 100)

  def newObjProp: Int = newObj

  def assignProp: Int = newObjProp + assign

  def loadProp: Int = assignProp + load

  def storeProp: Int = loadProp + store

}

trait ProgramGenerator {
  def generate(): Program

  def genQuery: Cell
}


class SimpleProgramGenerator(seed: Int, vars: Int, instructions: Int, fields: Int, distribution: Distribution) extends ProgramGenerator {


  def this(seed: Int, vars: Int, instructions: Int, fields: Int) = {
    this(seed, vars, instructions, fields, Distribution())
  }

  private val rng = new Random(seed)
  private var tokens = 0

  def genQuery: Int = rng.nextInt(vars)

  def generate(): Program = {
    val p: List[Instruction] = (0 to instructions).foldLeft(List[Instruction]())((p, _) => genInsn() :: p)
    Program(p)
  }

  private def genInsn(): Instruction = {
    val i = rng.nextInt(100)
    i match {
      case x if x <= distribution.newObjProp =>
        New(genVar(), genToken())
      case x if x <= distribution.assignProp =>
        Assign(genVar(), genVar())
      case x if x <= distribution.loadProp =>
        Load(genVar(), genVar(), genField())
      case x if x <= distribution.storeProp =>
        Store(genVar(), genField(), genVar())
    }
  }

  private def genVar(): Var =
    rng.nextInt(vars)

  private def genToken(): Token =
    tokens += 1
    tokens

  private def genField(): Field =
    rng.nextInt(fields)

}

class AlwaysInitializedProgramGenerator(seed: Int, size: Int, vars: Int, fields: Int) extends ProgramGenerator {

  val rng = new Random(seed)
  private val initializedVars = mutable.Set[Int]()

  var nextToken = 0

  override def generate(): Program =
    val insn = ArrayBuffer[Instruction]()
    for _ <- 0 until size do
      val i = rng.nextInt(4)
      i match
        case x if x <= 1 =>
          val left = genVarAndInitialize(insn)
          val right = genVarAndInitialize(insn)
          insn.addOne(Assign(left, right))
        case x if x <= 2 =>
          val dst = genVarAndInitialize(insn)
          val base = genVarAndInitialize(insn)
          val field = genField()
          insn.addOne(Load(dst, base, field))
        case x if x <= 3 =>
          val src = genVarAndInitialize(insn)
          val base = genVarAndInitialize(insn)
          val field = genField()
          insn.addOne(Store(base, field, src))
    Program(insn.toList)



  def genQuery: Int = rng.nextInt(vars)

  private def genVarAndInitialize(insn: ArrayBuffer[Instruction]): Int = {
    val v = rng.nextInt(vars)
    if !initializedVars(v) then
      insn.addOne(New(v, nextToken))
      nextToken += 1
      initializedVars.add(v)
    v
  }
  private def genField(): Int = rng.nextInt(fields)
  
}