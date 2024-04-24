import scala.util.Random

class Distribution(val newObj: Int = 20, val assign: Int = 40, val load: Int = 20, val store: Int = 20) {

  assert(storeProp == 100)
  def newObjProp: Int = newObj
  def assignProp: Int = newObjProp + assign
  def loadProp: Int = assignProp + load
  def storeProp: Int = loadProp + store

}

class ProgramGenerator(seed: Int, vars: Int, instructions: Int, fields: Int, distribution: Distribution) {


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
