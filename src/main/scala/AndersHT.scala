import scala.collection.mutable

class AndersHT {

  private val sol = mutable.Map[Cell, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)

  private val demand = mutable.Set[Cell]()
  private val tracked = mutable.Set[Token]()

  // Unprocessed demanded cells
  private val newDemand = mutable.Queue[Cell]()
  // Unprocessed tracked tokens
  private val newTracked = mutable.Queue[Token]()

  private val newPointsTo = mutable.Queue[(Cell, Token)]()

  private val newFlow = mutable.Queue[(Cell, Cell)]()

  // subset edges
  private val flow = mutable.Map[Cell, mutable.Set[Cell]]()

  // Maps non tracked tokens to the cells in which they are part of points-to sets
  private val flowsToNonTracked = mutable.Map[Token, mutable.Set[Cell]]()


  // ∀t ∈ ⟦y⟧ ...
  private val forAllTokensLoad = mutable.Map[Var, mutable.Set[(Var, Field)]]()
  private val forAllTokensStore = mutable.Map[Var, mutable.Set[(Field, Var)]]()

  private val forAllTokensTracked = mutable.Set[Var]()

  // ⟦x⟧ ∈ D ⇒ t ∈ ⟦x⟧
  private val ifNewDemanded = mutable.Map[Var, mutable.Set[Token]]()

  // ⟦x⟧ ∈ D ⇒ ⟦y⟧ ∈ D
  private val ifDemandedFlow = mutable.Map[Cell, mutable.Set[Cell]]()

  private val ifDemandedLoad = mutable.Map[Var, mutable.Set[Var]]()

  // If key has tracked token, demand and track all in value
  private val ifHasTrackedStore = mutable.Map[Var, mutable.Set[Var]]()


  def solve(p: Program, query: Var) = {
    for i <- p.getInstructions do
      visit(i)
    addDemand(query)

    while newDemand.nonEmpty || newTracked.nonEmpty do
      processNewDemand()
      processNewTracked()
      processNewPointsTo()
      processNewFlow()

  }

  private def visit(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        addMapSet(ifNewDemanded, x, t)
      case Assign(x, y) =>
        addMapSet(flow, y, x)
        addMapSet(ifDemandedFlow, x, y)
      case Load(x, y, f) =>
        addMapSet(forAllTokensLoad, y, (x, f))
        addMapSet(ifDemandedLoad, x, y)
      case Store(x, f, y) =>
        addMapSet(forAllTokensStore, x, (f, y))
        addMapSet(ifHasTrackedStore, y, x)

  }


  // Process constraints that are conditioned on ⟦x⟧ ∈ D
  private def processNewDemand(): Unit = {
    while (newDemand.nonEmpty) {
      val v = newDemand.dequeue()
      v match
        case x: Var =>
          // ⟦x⟧ ∈ D ⇒ t ∈ ⟦x⟧
          for t <- ifNewDemanded(x) do
            addToken(x, t)
          ifNewDemanded.remove(x)

          for y <- ifDemandedLoad(x) do
            addDemandAndTrackAll(y)
          ifDemandedLoad.remove(x)

        case _ =>

      // ⟦x⟧ ∈ D ⇒ ⟦y⟧ ∈ D ∧ ⟦y⟧ ⊆ ⟦x⟧
      for y <- ifDemandedFlow(v) do
        addDemand(y)
        addTokens(v, sol(y))
      ifDemandedFlow.remove(v)
    }
  }

  private def processNewTracked(): Unit = {
    while (newTracked.nonEmpty) {
      val t = newTracked.dequeue()
      // propagate t from all cells containing t, to all successors
      for c <- flowsToNonTracked(t) do
        for d <- flow(c) do
          addToken(d, t)

        // apply non-empty intersection constraint for all cells containing t, since t is now tracked
        c match
          case y: Var =>
            for x <- ifHasTrackedStore(y) do
              addDemandAndTrackAll(x)
            ifHasTrackedStore.remove(y)
          case _ =>

      flowsToNonTracked.remove(t)
    }
  }

  private def processNewPointsTo(): Unit = {
    while (newPointsTo.nonEmpty) {
      val (c, t) = newPointsTo.dequeue()
      c match
        case v: Var =>
          // Add corresponding subset edges by playing listeners
          for (x, f) <- forAllTokensLoad(v) do
            addFlow((t, f), x)
          for (f, y) <- forAllTokensStore(v) do
            addFlow(y, (t, f))
          // Track token if constraint variable is tracked
          if forAllTokensTracked(v) then
            addTracking(t)
          // Play non-empty intersection constraints if token was tracked
          if tracked(t) then
            for x <- ifHasTrackedStore(v) do
              addDemandAndTrackAll(x)
            ifHasTrackedStore.remove(v)
        case _ =>

          // Propagate tracked token
          if tracked(t) then
            for d <- flow(c) do
              addToken(d, t)
          // Propagate over all demand edges only
          else
            for d <- flow(c) do
              if demand(d) then
                addToken(d, t)
    }
  }

  private def processNewFlow(): Unit = {
    while newFlow.nonEmpty do
      val (c, d) = newFlow.dequeue()
      // If an edge from --> to was added, propagate any demand backwards and propagate tokens
      if demand(d) then
        addDemand(c)
        for t <- sol(c) do
          addToken(d, t)
      // Propagate tracked tokens over the new edge
      else
        addMapSet(ifDemandedFlow, d, c) // store that edge must be processed if demand arises
        for t <- sol(c) do
          if tracked(t) then
            addToken(d, t)
  }

  private def addDemand(c: Cell) = {
    if !demand(c) then
      demand.add(c)
      newDemand.enqueue(c)
  }

  private def addTracking(t: Token) = {
    if !tracked(t) then
      tracked.add(t)
      newTracked.enqueue(t)
  }

  private def addDemandAndTrackAll(x: Var): Unit = {
    if !forAllTokensTracked(x) then
      addDemand(x)
      forAllTokensTracked.add(x)
      for t <- sol(x) do
        addTracking(t)

  }

  private def addToken(x: Cell, t: Token): Unit = {
    if !sol(x).contains(t) then
      addMapSet(sol, x, t)
      newPointsTo.enqueue((x, t))
      if !tracked(t) then
        addMapSet(flowsToNonTracked, t, x)
  }

  // Add subset edge from --> to
  private def addFlow(from: Cell, to: Cell) = {
    if from != to && !flow(from).contains(to) then
      addMapSet(flow, from, to)
      newFlow.enqueue((from, to))
  }

  private def addTokens(x: Cell, t: mutable.Set[Token]) = {
    for tk <- t do
      addToken(x, tk)
  }

  def addMapSet[K, V](m: mutable.Map[K, mutable.Set[V]], k: K, v: V): Boolean = {
    if !m.contains(k) then
      val fresh = mutable.Set[V]()
      m += k -> fresh

    m(k).add(v)
  }
}
