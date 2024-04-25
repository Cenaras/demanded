import scala.collection.mutable

type Cell = Var | (Token, Field)
type Solution = mutable.Map[Cell, mutable.Set[Token]]

// TODO: Make a "naive solver" which uses this solver impl
trait Solver {

  var changed = true
  val sol: Solution = mutable.Map[Cell, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)

  protected def process(i: Instruction): Unit

  protected def addToken(x: Cell, t: Token): Unit = {
    // Create solution set if not seen before
    if !sol.contains(x) then
      val fresh = mutable.Set[Token]()
      sol += x -> fresh

    changed = changed | sol(x).add(t)
  }

  protected def propagate(to: Cell, from: Cell): Unit = {
    addTokens(to, sol(from))
  }

  protected def addTokens(to: Cell, tokens: mutable.Set[Token]): Unit = {
    for t <- tokens do
      addToken(to, t)
  }

  def printSolution(): Unit = {
    sol.foreach(f => {
      f._1 match
        case x: Var =>
          print(s"x$x: ")
          println(f._2)
        case y: (Token, Field) =>
          print(s"t${y._1}.f${y._2}: ")
          println(f._2)
    })
  }

  def naiveSolve(p: Program): Solution = {
    while (changed) {
      changed = false
      p.getInstructions.foreach(i => {
        process(i)
      })
    }
    sol
  }

}

trait ExhaustiveSolver extends Solver {
  def solve(p: Program): Solution
}

trait DemandedSolver extends Solver {
  def solve(p: Program, query: Cell): Solution
}



