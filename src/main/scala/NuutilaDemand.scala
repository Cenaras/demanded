import scala.collection.mutable

class NuutilaDemand {

  var i: Int = 0
  var d: mutable.Map[Node, Int] = mutable.Map[Node, Int]()
  var r: mutable.Map[Node, Node] = mutable.Map[Node, Node]()
  var c: mutable.Set[Node] = mutable.Set[Node]()
  var s: mutable.Stack[Node] = mutable.Stack[Node]()
  var t: mutable.ArrayBuffer[Node] = mutable.ArrayBuffer[Node]()


  // Receives as input a list of nodes currently demanded - performs backwards traversal to determine new demand
  def SCC_d(tidal: Tidal, demandedNodes: mutable.Set[Node], initial: Node): mutable.ArrayBuffer[Node] = {
    i = 0
    d = mutable.Map[Node, Int]().withDefaultValue(0) // 0 is unvisited
    r = mutable.Map[Node, Node]().withDefault(i => i) // default rep is identity
    c = mutable.Set[Node]()
    s = mutable.Stack[Node]()
    t = mutable.ArrayBuffer[Node]()

    val nodesToVisit = demandedNodes.toList.to(mutable.Queue)

    while (nodesToVisit.nonEmpty) {
      val v = nodesToVisit.dequeue()
      if d(v) == 0 then
        println(s"Dequeue and visiting ${v}")
        visit_d(v, tidal, nodesToVisit, initial)
    }

    for (v <- c) {
      if r(v) != v then
        unify_naive(v, r(v), tidal)
    }

    t
  }


  // A node is visited only if it is demanded
  // TODO: Need rep stuff?
  private def visit_d(v: Node, tidal: Tidal, nodesToVisit: mutable.Queue[Node], initial: Node): Unit = {

    if (!tidal.demand(v) || v == initial) {
      tidal.addDemand(v)
      // Piggy-back on SCC and add tokens if newEdge is present
      for t <- tidal.revNewEdges(v) do
        println(s"Piggy-back adding token ${t} to demanded ${v}")
        tidal.addToken(v, t)

      // Load constraints all request demand backwards as well
      for (c <- tidal.loads(v)) {
        println(s"Visiting load ${c} on ${v}")
        if c.x == v then
          println(s"Enqueuing ${c.y}")
          nodesToVisit.enqueue(c.y)
        for (t <- tidal.sol(c.y)) do
          println(s"∀t in load: Enqueuing ${t}.${c.f}")
          nodesToVisit.enqueue((t, c.f))
      }

    }

    // SCC traversal over reversed subset edges
    i += 1
    d(v) = i
    r(v) = v

    for (w <- tidal.revSubsetEdges(v)) {
      if d(w) == 0 then
        visit_d(w, tidal, nodesToVisit, initial)

      if !c(w) then
        if d(r(v)) >= d(r(w)) then
          r(v) = r(w)
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
  
  
  def SCC_t(tidal: Tidal): mutable.ArrayBuffer[Node] = {
    i = 0
    d = mutable.Map[Node, Int]().withDefaultValue(0) // 0 is unvisited
    r = mutable.Map[Node, Node]().withDefault(i => i) // default rep is identity
    c = mutable.Set[Node]()
    s = mutable.Stack[Node]()
    t = mutable.ArrayBuffer[Node]()

    val nodesToVisit = mutable.Queue[Node]() 
    
    
    // Start from tracked new edges
    
    for t <- tidal.tracked do
      val nodes = tidal.newEdges(t)
      for v <- nodes do
        nodesToVisit.enqueue(v)
    
    while nodesToVisit.nonEmpty do
      val v = nodesToVisit.dequeue()
      if d(v) == 0 then
        visit_t(v, tidal, nodesToVisit)
    
    for v <- c do 
      if r(v) != v then
        unify_naive(v, r(v), tidal)
    
    t
    
    
  }
  
  private def visit_t(v: Node, tidal: Tidal, nodesToVisit: mutable.Queue[Node]): Unit = {
    // Find all nodes reachable from tracked new edges
    
    i += 1
    d(v) = i
    r(v) = v
    
    for (w <- tidal.subsetEdges(v)) {
      if d(w) == 0 then
        visit_t(w, tidal, nodesToVisit)
      
      if !c(w) then
        if d(r(v)) >= d(r(w)) then
          r(v) = r(w)
    }
    
    if (r(v) == v) {
      c.add(v)
      
      var break = false
      while(s.nonEmpty && !break) {
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

  private def unify_naive(n: Node, rep: Node, tidal: Tidal) = {

    for t <- tidal.sol(n) do
      tidal.addToken(rep, t) // Unify points-to set...

    // ... subset edges ...
    for (w <- tidal.subsetEdges(n)) {
      tidal.addSubsetEdge(rep, w)
    }

    // ... and complex constraints
    for (c <- tidal.loads(n)) {
      tidal.addLoad(rep, c)
    }
    for (c <- tidal.stores(n)) {
      tidal.addStore(rep, c)
    }

    tidal.repMap(n) = rep

  }

}
