import scala.collection.mutable



/** Copy of Heintze-Tardieu with slight modifications. Tokens are only tracked once a write is seen.
 * Tracked tokens are only propagated, if they can reach a write operation. */
class ImprovedHeintzeTardieu extends DemandedSolver {

  val d = mutable.Set[Cell]()
  val r = mutable.Set[Token]()

  val write_reachable = mutable.Set[Cell]()


  private def addDemand(c: Cell): Unit =
    changed = changed | d.add(c)

  private def trackToken(t: Token): Unit =
    changed = changed | r.add(t)

  private def trackTokens(tokens: mutable.Set[Token]): Unit =
    for t <- tokens do
      trackToken(t)

  private def addWriteReachable(x: Cell): Unit = {
    changed |= write_reachable.add(x)
  }


  private def propagateTracked(to: Cell, from: Cell): Unit = {
    if write_reachable(to) then addTokens(to, sol(from).intersect(r))
  }

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
        propagateTracked(x, y)
        if write_reachable(x) then addWriteReachable(y)

      case Load(x, y, f) =>
        if write_reachable(x) then addWriteReachable(y)

        if d(x) then
          addDemand(y)
          for t <- sol(y) do
            addDemand((t, f))
            propagate(x, (t, f)) // don't always track y, only if a store is present.
        for t <- sol(y) do
          if write_reachable(x) then addWriteReachable((t, f))
          propagateTracked(x, (t, f))

      case Store(x, f, y) =>
        addWriteReachable(x)
        addWriteReachable(y)
        // Track every demanded token, only if a store is present
        for c <- d do
          c match
            case a: Var =>
            case b: (Token, Field) => trackToken(b._1)
        for t <- List.from(sol(x)) do
          if d(t, f) then
            addDemand(y)
            propagate((t, f), y)
          propagateTracked((t, f), y)
        if sol(y).intersect(r).nonEmpty then
          addDemand(x)
          trackTokens(sol(x))
  }

  override def solve(p: Program, query: Cell): Solution = {
    addDemand(query)
    naiveSolve(p)
  }

  override def cost: Int = {
    var cost = 0
    for (cell, solSet) <- sol do
      cost += solSet.size

    cost += d.size
    cost += r.size
    cost += write_reachable.size

    cost
  }

}
