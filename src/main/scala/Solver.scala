import scala.collection.mutable

type Cell = Var | (Token, Field)
type Solution = mutable.Map[Cell, mutable.Set[Token]]

trait Solver {

  var changed = true
  // TODO: Can we override the map lookup sol(x) to create a new value AND bind x to it?
  val sol: Solution = mutable.Map[Cell, mutable.Set[Token]]().withDefaultValue(mutable.Set())


  def solve(p: Program): Solution = {
    while (changed) {
      changed = false

      p.getInstructions.foreach(i => {
        process(i)
      })
    }
    sol
  }


  protected def process(i: Instruction): Unit

  protected def addToken(x: Cell, t: Token): Unit = {
    // Initialise solution for x, if not present
    val xSol = sol.getOrElseUpdate(x, mutable.Set())

    changed = xSol.add(t)
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
          print(s"x${y._1}.f${y._2}: ")
          println(f._2)
    })
  }


}


