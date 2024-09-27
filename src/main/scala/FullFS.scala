import scala.collection.mutable

// TODO: Implement chain program with relation rules as done in Figure 3 of FullFS paper. Compare with MS and FullFS
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
        if d(x) then 
          addDemand(y) // (6)
          for (t <- sol(y)) do 
            addTrackWrite(t) // (7)
            propagate(x, (t, f)) // (8)
        
        for (t <- sol(y)) do 
          if r_read(t) then
            addTokens(x, sol(t, f).intersect(r_any)) // (9)
      
      
      case Store(x, f, y) =>
        for (t <- sol(x)) do 
          if r_write(t) then 
            addDemand(y) // (10)
            propagate((t, f), y) // (11)
          
        if sol(y).intersect(r_any).nonEmpty then
          addDemand(x) // (12)
          for (t <- sol(x)) do 
            propagate((t,f), y) // (13)
            addTrackRead(t) // (13)
          
  }

  override def solve(p: Program, query: Cell): Solution = {
    addDemand(query)
    naiveSolve(p)
  }

  override def cost: Int = 0

}
