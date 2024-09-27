import scala.collection.mutable


// TODO: Force CSolver's to always have a specific field
abstract class SVFSolver(res: SVFResult) {
  
  var changed = true
  val sol: CSolution = mutable.Map[Int, mutable.Set[Int]]().withDefaultValue(mutable.Set.empty)

  protected def process(i: CInstruction): Unit


  def addPts(x: Int, y: Int): Unit = {
    if !res.nodes.contains(x) then
      return

    if !sol.contains(x) then
      val fresh = mutable.Set[Int]()
      sol += x -> fresh

    changed |= sol(x).add(y)
  }
  
  def propagate(from: Int, to: Int): Unit = {
    sol(from).foreach(c => addPts(to, c))
  }
  

}

abstract class ExhaustiveSVFSolver(res: SVFResult) extends SVFSolver(res) {
  def solve(): CSolution
}

abstract class DemandedSVFSolver(res: SVFResult) extends SVFSolver(res){
  val demanded = mutable.Set[Int]()
  val tracked = mutable.Set[Int]()
  
  
  def addDemand(x: Int) = {
    changed |= demanded.add(x)
  }
  
  def addTracked(x: Int) = {
    changed |= tracked.add(x)
  }
  
  
  def solve(query: Int): CSolution
  
  
  
  
  
}



