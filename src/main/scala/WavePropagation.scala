import scala.collection.mutable


class WpSolution(sol_set: mutable.Map[Node, mutable.Set[Token]], repMap: mutable.Map[Node, Node]){

  def sol(x: Cell): mutable.Set[Token] = {
    sol_set(repMap(x))
  }

  def keySet: collection.Set[Node] = sol_set.keySet
  def printSol() = {
    for node <- sol_set.keySet do
      print(s"x${node} -> ")
      for t <- sol(node) do
        print(s"t$t")
      println()
  }


}


// Standalone
class WavePropagation {

  private type Node = Cell
  private type Complex = Load | Store



  val nodes = mutable.Set[Node]()
  val repMap: mutable.Map[Node, Node] = mutable.Map[Node, Node]().withDefault(i => i) // Mapping nodes to representatives


  val subsetEdges = mutable.Map[Node, mutable.Set[Node]]().withDefaultValue(mutable.Set.empty)
  val complex = mutable.Map[Node, mutable.Set[Complex]]().withDefaultValue(mutable.Set.empty)
  val sol = mutable.Map[Node, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)

  def solve(p: Program): WpSolution = {
    initGraph(p)

    val nuutila = Nuutila()

    var changed = true

    while (changed) {
      val topoSort = nuutila.SCC(this)
      propagateTokens(topoSort)
      changed = insertEdges()
    }

    WpSolution(sol, repMap)

  }


  private def propagateTokens(topoSort: mutable.ArrayBuffer[Node]): Unit = {
    val size = topoSort.size

    for i <- 0 until size do
      val top = size - i - 1

      val v = repMap(topoSort(top))

      topoSort.remove(top)

      // propagate tokens along edge v --> w
      for (w <- subsetEdges(v)) {
        addTokens(w, sol(v))
      }
  }

  private def insertEdges(): Boolean = {
    var changed = false

    for (n <- nodes) {
      val rep = repMap(n)
      val complex_constraints = complex(rep)

      for (c <- complex_constraints) {
        changed |= solveComplex(c)
      }
    }

    changed
  }


  private def solveComplex(c: Complex): Boolean = {
    var changed = false

    c match
      case Load(x, y, f) =>
        val rep_x = repMap(x)
        val rep_y = repMap(y)
        // ∀t ∈ ⟦y⟧: ...
        val sol_y = sol(rep_y)
        for (t <- sol_y) {
          // ... add node if not present
          val tf = repMap(t, f)
          if !nodes(tf) then
            nodes.add(tf)
            changed = true

          // ... add subset edge ⟦t.f⟧ ⊆ ⟦x⟧
          changed |= addMapSet(subsetEdges, tf, rep_x)
          // ... and propagate tokens
          changed |= addTokens(rep_x, sol(tf))
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
          changed |= addMapSet(subsetEdges, rep_y, tf)
          // ... and propagate tokens
          changed |= addTokens(tf, sol(rep_y))
        }

    changed
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

  def addTokens(x: Node, tokens: mutable.Set[Token]): Boolean = {
    var changed = false
    for (t <- tokens)
      changed |= addToken(x, t)
    changed
  }

  private def addSubsetEdge(from: Node, to: Node): Unit = {
    addMapSet(subsetEdges, from, to)
  }

  private def addComplex(node: Node, complex: Complex): Unit = {
    addMapSet(this.complex, node, complex)
  }


  private def initGraph(program: Program): Unit = {
    program.getInstructions.foreach(p => {

      //      println(s"Processing $p")

      p match
        case a: New =>
          nodes.add(a.x)
          addToken(a.x, a.t)
        case b: Assign =>
          nodes.add(b.x)
          nodes.add(b.y)
          addSubsetEdge(b.y, b.x)
        case c: Load =>
          nodes.add(c.y)
          nodes.add(c.x)
          addComplex(c.y, c)
        case d: Store =>
          nodes.add(d.x)
          nodes.add(d.y)
          addComplex(d.x, d)
    })

  }


}
