import scala.collection.mutable

class HeintzeTardieu extends DemandedSolver {

  val d = mutable.Set[Cell]()
  val r = mutable.Set[Token]()


  private def addDemand(c: Cell): Unit =
    changed = changed | d.add(c)

  private def trackToken(t: Token): Unit =
    changed = changed | r.add(t)

  private def trackTokens(tokens: mutable.Set[Token]): Unit =
    for t <- tokens do
      trackToken(t)

  override def process(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        if d(x) then
          addToken(x, t)
        if r(t) then
          addToken(x, t)

      case Assign(x, y) =>
        if d(x) then
          addDemand(y)
          propagate(x, y)
        addTokens(x, sol(y).intersect(r))

      case Load(x, y, f) =>
        if d(x) then
          addDemand(y)
          for t <- sol(y) do
            trackToken(t)
            addDemand((t, f))
            propagate(x, (t, f))
        for t <- sol(y) do
          addTokens(x, sol((t, f)).intersect(r))

      case Store(x, f, y) =>
        for t <- List.from(sol(x)) do
          if d(t, f) then
            addDemand(y)
            propagate((t, f), y)
          addTokens((t, f), sol(y).intersect(r))
        if sol(y).intersect(r).nonEmpty then
          addDemand(x)
          trackTokens(sol(x))
  }

  override def solve(p: Program, query: Cell): Solution = {
    addDemand(query)
    while (changed) {
      changed = false
      p.getInstructions.foreach(i => {
        process(i)
      })
    }
    sol
  }

}
