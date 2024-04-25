import scala.collection.mutable

class FullFS extends DemandedSolver {

  val d = mutable.Set[Cell]()
  val r_any = mutable.Set[Token]()
  val r_write = mutable.Set[Token]()
  val r_read = mutable.Set[Token]()


  private def addDemand(cell: Cell): Unit = {
    changed |= d.add(cell)
  }

  private def addTrackAny(t: Token): Unit = {
    changed |= r_any.add(t)
  }
  // r_any is "any" reason, so also add it when adding due to specific reasons
  private def addTrackRead(t: Token): Unit = {
    changed |= r_read.add(t)
    changed |= r_any.add(t)
  }
  private def addTrackWrite(t: Token): Unit = {
    changed |= r_write.add(t)
    changed |= r_any.add(t)
  }


  override protected def process(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        if d(x) then addToken(x, t) // (1)
        if r_any(x) then addToken(x, t) // (2)
      
      case Assign(x, y) =>
        if d(x) then 
          addDemand(y) // (3)  
          propagate(x, y) // (4)
          
        addTokens(x, sol(y).intersect(r_any)) // (5)
      
      case Load(x, y, f) =>

      
      case Store(x, f, y) =>
  }

  override def solve(p: Program, query: Cell): Solution = {
    addDemand(query)
    naiveSolve(p)
  }
}
