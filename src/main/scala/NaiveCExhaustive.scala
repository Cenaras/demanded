import scala.collection.mutable

// TODO: Generalize into trait similar to the Java solver

class NaiveCExhaustive {

  val sol = mutable.Map[Cell, mutable.Set[Cell]]().withDefaultValue(mutable.Set.empty)
  var changed = true

  def solve(p: CProgram): CSolution = {
    while (changed) {
      changed = false
      p.getInstructions.foreach(i => {
        process(i)
      })
    }
    sol
  }


  def process(i: CInstruction): Unit = {
    i match
      case AddrOf(x, y) =>
        addPts(x, y)
      case Copy(x, y) =>
        propagate(y, x)
      case CLoad(x, y) =>
        for c <- sol(y) do 
          propagate(c, x)
      case CStore(x, y) =>
        for c <- sol(x) do 
          propagate(y, c)
      case Gep(dst, base, offset) =>
        throw new Error("GEP unsupported")
  }
  

  def addPts(x: Cell, y: Cell): Unit = {
    if !sol.contains(x) then
      val fresh = mutable.Set[Cell]()
      sol += x -> fresh
    changed |= sol(x).add(y)
  }

  def propagate(from: Cell, to: Cell): Unit = {
    sol(from).foreach(c => addPts(to, c))
  }

  

}
