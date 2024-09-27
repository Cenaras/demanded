import scala.collection.mutable

private type Node = Cell
private type Complex = Load | Store

// This is just a stand-alone test
class Tidal {


  val sol = mutable.Map[Node, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)
  var changed = true


  val demanded = mutable.Set[Node]()
  val tracked = mutable.Set[Token]()

  val nodes = mutable.Set[Node]()
  val repMap: mutable.Map[Node, Node] = mutable.Map[Node, Node]().withDefault(i => i) // Mapping nodes to representatives

  val ifDemandNew = mutable.Map[Node, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty) // new edges
  val token2New = mutable.Map[Token, mutable.Set[Node]]().withDefaultValue(mutable.Set.empty)


  // Backwards demand for loads
  val ifDemandLoad = mutable.Map[Node, mutable.Set[(Node, Field)]]().withDefaultValue(mutable.Set.empty) // for x = y.f, attach to x
  val loadConstraints = mutable.Map[Node, mutable.Set[(Node, Field)]]().withDefaultValue(mutable.Set.empty)


  // need both the store constraint on the dst node for edge insert and on the src node for non-empty intersection
  val storeConstraints = mutable.Map[Node, mutable.Set[(Node, Field)]]().withDefaultValue(mutable.Set.empty)
  val ifHasTrackedStore = mutable.Map[Node, mutable.Set[(Node, Field)]]().withDefaultValue(mutable.Set.empty)

  val edges = mutable.Map[Node, mutable.Set[Node]]().withDefaultValue(mutable.Set.empty) // Do not propagate over unconditionally!
  val ifDemandedSubset = mutable.Map[Node, mutable.Set[Node]]().withDefaultValue(mutable.Set.empty) // reverse subset for demand


  def solve(p: Program, q: Var): Solution = {
    initGraph(p)
    addDemand(q)

    val nuutila = NuutilaDemand()
    while (changed) {
      changed = false
      //      val topoSort = nuutila.SCC_d(this, demanded, q)
      val topoSort = nuutila.explore(this, demanded)
//      println(topoSort)
      propagate(topoSort)

      trackedPropagation()

      //      println(s"topoSort: ${topoSort}")

      insertEdges(topoSort)
    }


    //    println("Solution: ")
    //    println(sol)

    println("Demanded")
    println(demanded)
    println("Tracked")
    println(tracked)
    println("Subsets")
    println(edges)

//    println("Store constraints")
//    println(storeConstraints)

    sol

  }

  private def propagate(t: mutable.ArrayBuffer[Node]) = {
    val ordering = t.clone()

    val size = ordering.size

    for i <- 0 until size do
      val node = ordering(size - i - 1)

      // Propagate to nodes in the toposort only (as those are the demanded nodes)
      for w <- edges(node) do
        if ordering.contains(w) then
          addTokens(w, sol(node))
  }

  private def trackedPropagation() = {

    val queue = mutable.Queue[(Node, Token)]()
    val visited = mutable.Set[(Node, Token)]() // since if not deduplicating, we need to handle these cases

    for t <- tracked do
      for v <- token2New(t) do
        queue.enqueue((v, t))

    while queue.nonEmpty do
      val (v, t) = queue.dequeue()

      if (!visited((v, t))) {
//        println(s"Adding x${v}, t${t} to visited")
        visited.add((v, t))

        // propagate tracked over subset edges
        for w <- edges(v) do
          queue.enqueue((w, t))
          addToken(w, t)

        // apply tracked in store
        for (y, f) <- ifHasTrackedStore(v) do
//          println(s"x${v} has a tracked token, apply rule on x${y}.f${f}")
          addDemand(y)
          for t1 <- sol(y) do
            addTracked(t1)

        // propagate over load edges
        for (y, f) <- loadConstraints(v) do
          for t <- sol(v) do
            val tf = (t, f)
            addTokens(y, sol(tf).intersect(tracked))

        // propagate over store edges (hotfix) and trigger ∀ constraints for demanded t.f (hotfix)
        for (y, f) <- storeConstraints(v) do
//          println(s"Tracked Store constraint ${v} --> ${y} over ${f}")
          for t <- sol(v) do
            val tf = (t, f)
            addTokens(tf, sol(y).intersect(tracked))
            if demanded(tf) then
//              println(s"Hotfix edge from ${y} --> ${tf}")
              addSubset(y, tf)
              addDemand(y)


      }




  }

  private def insertEdges(t: mutable.ArrayBuffer[Node]) = {
    while t.nonEmpty do
      val node = t.remove(0)

      // Add load edges
      for (x, f) <- ifDemandLoad(node) do
        //        println(s"Load edge ${x} --> ${node} over ${f}")
        for t <- sol(x) do
          // add node (if not present) and demand it
          val tf = (t, f)
          nodes.add(tf)
          addDemand(tf)
          addSubset(tf, node)
          addTracked(t)


      for (y, f) <- storeConstraints(node) do
        for t <- sol(node) do
          val tf = (t, f)
          if demanded(tf) then
//            println(s"Demanding ${y} and adding ${y} --> ${tf}")
            addDemand(y)
            addSubset(y, tf)
  }


  def addDemand(x: Node) =
    changed |= demanded.add(x)


  def addTracked(t: Token) =
    changed |= tracked.add(t)


  def addToken(x: Node, t: Token) = {
    addMapSet(sol, x, t)
  }

  def addTokens(x: Node, tokens: mutable.Set[Token]) = {
    for t <- tokens do
      addToken(x, t)
  }

  private def initGraph(program: Program) = {
    program.getInstructions.foreach {
      case a: New =>
        nodes.add(a.x)
        addMapSet(ifDemandNew, a.x, a.t)
        addMapSet(token2New, a.t, a.x)
      case b: Assign =>
        nodes.add(b.x)
        nodes.add(b.y)
        addSubset(b.y, b.x)
        addIfDemandedSubset(b.x, b.y)

      case c: Load =>
        nodes.add(c.x)
        nodes.add(c.y)
        addLoadConstraint(c.y, c.f, c.x)

      case d: Store =>
        nodes.add(d.x)
        nodes.add(d.y)
        addStoreConstraint(d.x, d.f, d.y)
    }
  }

  private def addSubset(from: Node, to: Node): Unit = {
    addMapSet(edges, from, to)
  }

  private def addIfDemandedSubset(to: Node, from: Node): Unit = {
    addMapSet(ifDemandedSubset, to, from)
  }

  // for load x = y.f
  private def addLoadConstraint(y: Node, f: Field, x: Node) = {
    addMapSet(ifDemandLoad, x, (y, f)) // used to propagate demand from x to y
    addMapSet(loadConstraints, y, (x, f)) // forwards propagation
  }

  // for store x.f = y: Handles both types of constraints
  private def addStoreConstraint(x: Node, f: Field, y: Node) = {
    addMapSet(storeConstraints, x, (y, f))
    addMapSet(ifHasTrackedStore, y, (x, f))
  }

  def addMapSet[K, V](m: mutable.Map[K, mutable.Set[V]], k: K, v: V): Unit = {
    if !m.contains(k) then
      val fresh = mutable.Set[V]()
      m += k -> fresh

    changed |= m(k).add(v)
  }
}
