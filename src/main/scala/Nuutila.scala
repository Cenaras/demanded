import scala.collection.mutable

private type Node = Cell

class Nuutila {


  var i: Int = 0
  var d: mutable.Map[Node, Int] = mutable.Map[Node, Int]()
  var r: mutable.Map[Node, Node] = mutable.Map[Node, Node]()
  var c: mutable.Set[Node] = mutable.Set[Node]()
  var s: mutable.Stack[Node] = mutable.Stack[Node]()
  var t: mutable.ArrayBuffer[Node] = mutable.ArrayBuffer[Node]()


  def SCC(wp: WavePropagation): mutable.ArrayBuffer[Node] = {
    // Prepare for new iteration
    i = 0
    d = mutable.Map[Node, Int]().withDefaultValue(0) // 0 is unvisited
    r = mutable.Map[Node, Node]().withDefault(i => i) // default rep is identity
    c = mutable.Set[Node]()
    s = mutable.Stack[Node]()
    t = mutable.ArrayBuffer[Node]()

    for v <- wp.nodes do
      if d(v) == 0 then
        visit(v, wp.subsetEdges)

    for v <- wp.nodes do
      if r(v) != v then
        unify_naive(v, r(v), wp)
    t
  }

  private def visit(v: Node, edges: mutable.Map[Node, mutable.Set[Node]]): Unit = {
    i += 1
    d(v) = i
    r(v) = v

    for (w <- edges(v)) {
      if d(w) == 0 then
        visit(w, edges)

      if !c(w) then
        if d(r(v)) >= d(r(w)) then
          r(v) = r(w)
      // in cylces add?
    }


    if (r(v) == v) {
      c.add(v)

      var break = false

      while (s.nonEmpty && !break) {
        val w = s.top
        if d(w) <= d(v) then
          break = true
        else {
          s.pop()
          c.add(w)
          r(w) = v
        }
      }
      t.addOne(v)
    } else {
      s.push(v)
    }

  }


  private def find(n: Node, wp: WavePropagation): Node = {
    var rep = wp.repMap(n)
    if rep != n then
      rep = find(rep, wp) // path compression
      wp.repMap(n) = rep

    rep

  }


  //
  private def unify(n: Node, rep: Node, wp: WavePropagation) = {
    val v = find(n, wp)
    val rep = find(r(v), wp)

    if (v != rep) {
    }
  }

  // Unify n into rep
  private def unify_naive(n: Node, rep: Node, wp: WavePropagation) = {

    for t <- wp.sol(n) do
      wp.addToken(rep, t) // Unify points-to set...

    // ... subset edges ...
    for (w <- wp.subsetEdges(n)) {
      wp.addMapSet(wp.subsetEdges, rep, w)
    }

    // ... and complex constraints
    for (c <- wp.complex(n)) {
      wp.addMapSet(wp.complex, rep, c)
    }

    wp.repMap(n) = rep

  }


}
