import scala.collection.mutable

private type Node = Cell
private type Complex = Load | Store

// This is just a stand-alone test
class Tidal {


  val nodes = mutable.Set[Node]()
  val newNodes = mutable.Set[Token]()

  val repMap: mutable.Map[Node, Node] = mutable.Map[Node, Node]().withDefault(i => i) // Mapping nodes to representatives
  val subsetEdges = mutable.Map[Node, mutable.Set[Node]]().withDefaultValue(mutable.Set.empty)
  val revSubsetEdges = mutable.Map[Node, mutable.Set[Node]]().withDefaultValue(mutable.Set.empty)

  // Will never have incoming edges, so can never form an SCC
  val newEdges = mutable.Map[Token, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)
  val revNewEdges = mutable.Map[Node, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)
  val loads = mutable.Map[Node, mutable.Set[Load]]().withDefaultValue(mutable.Set.empty)
  val stores = mutable.Map[Node, mutable.Set[Store]]().withDefaultValue(mutable.Set.empty)
  val sol = mutable.Map[Node, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)


  val demand = mutable.Set[Node]()
  val tracked = mutable.Set[Token]()

  var changed = true

  def solve(p: Program, q: Cell): WpSolution = {
    initGraph(p)
    addDemand(q)
    // Add initial new tokens
    addTokens(q, revNewEdges(q))


    val nuutila = NuutilaDemand()


    while (changed) {
      changed = false

      val topo_d = nuutila.SCC_d(this, demand, q)
      val topo_d_copy = topo_d.clone()
      println(s"TopoSort: ${topo_d}")
      propagate_d(topo_d)

      val topo_t = nuutila.SCC_t(this)
      println(s"TopoTracked: ${topo_t}")

      // TODO: Propagate tracked - insert edges
      // We cannot add stores edges naively - we must check for demand
//      insertEdges(topo_d_copy)

    }

    println("Demanded: ")
    println(demand)

    println("Solution")
    val solution = WpSolution(sol, repMap)
    solution.printSol()
    solution
  }

  private def propagate_d(topoSort: mutable.ArrayBuffer[Node]): Unit = {
    val size = topoSort.size

    for i <- 0 until size do
      val top = size - i - 1

      val v = repMap(topoSort(top))
      topoSort.remove(top)

      // propagate tokens along edge v --> w  TODO: Is the demand check needed? Shouldn't SCC ensure only demanded edges are given?
      val outgoing = subsetEdges(v).intersect(demand)
      for (w <- outgoing)
        addTokens(w, sol(v))

  }

  private def insertEdges(nodes: mutable.ArrayBuffer[Node]): Unit = {
    // Iterate all nodes, those that are non-demanded/non-tracked should have empty points-to sets...
    // We could iterate smarter...
    for (n <- nodes) {
      val rep = repMap(n)
      val load_constraints = loads(rep)
      val store_constraints = stores(rep)

      for c <- load_constraints do
        solveComplex(c)
      for c <- store_constraints do
        solveComplex(c)
    }
  }


  private def solveComplex(c: Load | Store): Unit = {
    c match
      case Load(x, y, f) =>
        val rep_x = repMap(x)
        val rep_y = repMap(y)

        val sol_y = sol(rep_y)
        for (t <- sol_y) {
          val tf = repMap(t, f)
          if !nodes(tf) then
            nodes.add(tf)
            changed = true

          // ... add subset edge ⟦t.f⟧ ⊆ ⟦x⟧
          println(s"Adding edge t${t}.f${f} --> x${rep_x}")
          addSubsetEdge(tf, rep_x)
          // ... and propagate tokens
          addTokens(rep_x, sol(tf))
        }

      case Store(x, f, y) =>
        val rep_x = repMap(x)
        val rep_y = repMap(y)

        // ∀t ∈ ⟦x⟧
        val sol_x = sol(rep_x)
        for (t <- sol_x) {
          // ... add node
          val tf = repMap(t, f)
          if !nodes(tf) then
            nodes.add(tf)
            changed = true

          // ... add subset edge ⟦y⟧ ⊆ ⟦t.f⟧
          println(s"Adding edge x${rep_y} --> t${t}.f${f} ")
          addSubsetEdge(rep_y, tf)
          // ... and propagate tokens
          addTokens(tf, sol(rep_y))

        }
  }

  private def initGraph(program: Program): Unit =
    program.getInstructions.foreach {
      case a: New =>
        nodes.add(a.x)
        newNodes.add(a.t)
        addMapSet(revNewEdges, a.x, a.t)
        addMapSet(newEdges, a.t, a.x)

      case b: Assign =>
        nodes.add(b.x)
        nodes.add(b.y)
        addSubsetEdge(b.y, b.x)

      case c: Load =>
        nodes.add(c.y)
        nodes.add(c.x)
        addLoad(c.x, c)

      case d: Store =>
        nodes.add(d.x)
        nodes.add(d.y)
        addStore(d.x, d)
    }

  def addMapSet[K, V](m: mutable.Map[K, mutable.Set[V]], k: K, v: V): Boolean = {
    if !m.contains(k) then
      val fresh = mutable.Set[V]()
      m += k -> fresh

    m(k).add(v)
  }

  def addToken(x: Node, t: Token): Boolean = {
    addMapSet(sol, repMap(x), t)
  }

  def addTokens(x: Node, tokens: mutable.Set[Token]) = {
    for (t <- tokens)
      changed |= addToken(x, t)
  }

  def addDemand(n: Node) = {
    println(s"Adding demand to ${n}")
    changed |= demand.add(n)
  }

  def addTracking(t: Token) = {
    changed |= tracked.add(t)
  }

  def addSubsetEdge(from: Node, to: Node): Unit = {
    changed |= addMapSet(subsetEdges, from, to)
    changed |= addMapSet(revSubsetEdges, to, from)
  }

  def addLoad(node: Node, complex: Load): Unit = {
    addMapSet(this.loads, node, complex)
  }

  def addStore(node: Node, complex: Store): Unit = {
    addMapSet(this.stores, node, complex)
  }

}
