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

      case Assign(left, right) =>
        if d(left) then
          addDemand(right)
          propagate(left, right)
        addTokens(left, sol(right).intersect(r))

      case Load(res, base, field) =>
        if d(res) then
          addDemand(base)
          for t <- sol(base) do
            addDemand((t, field))
            propagate(res, (t, field))
        for t <- sol(base) do
          addTokens(res, sol(base).intersect(r))

      case Store(base, field, value) =>
        for t <- sol(base) do
          if d(t, field) then
            addDemand(value)
            propagate((t, field), value)
          addTokens((t, field), sol(value).intersect(r))
        if (sol(value).intersect(r).nonEmpty) then
          addDemand(base)
          trackTokens(sol(base))
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
