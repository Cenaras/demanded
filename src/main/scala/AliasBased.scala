import com.sun.org.apache.bcel.internal.generic.LoadInstruction

import scala.collection.mutable

class AliasBased extends DemandedSolver {

  // Demanded variables
  val d = mutable.Set[Cell]()
  // Alias queries
  val a = mutable.Set[Cell]()
  // Alias relations
  val alias = mutable.Map[Cell, mutable.Set[Cell]]().withDefaultValue(mutable.Set.empty)
  val load2Store = mutable.Map[Load, mutable.Set[Store]]().withDefaultValue(mutable.Set.empty)
  val store2load = mutable.Map[Store, mutable.Set[Load]]().withDefaultValue(mutable.Set.empty)

  private def addDemand(c: Cell): Unit =
    changed |= d.add(c)

  private def addAliasQuery(c: Cell): Unit =
    changed |= a.add(c)

  private def addAlias(x: Cell, y: Cell) =
    def addIfEmpty(x: Cell) =
      if !alias.contains(x) then
        val fresh = mutable.Set[Cell]()
        fresh.add(x) // reflexive
        alias += x -> fresh

    addIfEmpty(x)
    addIfEmpty(y)

    // symmetric and transitive
    addAliasTransitive(x, y)
    addAliasTransitive(y, x)


 /** Transitively add alias relations. */
  private def addAliasTransitive(x: Cell, y: Cell): Unit = {
    if alias(x).contains(y) then
      return
    changed |= alias(x).add(y)
    for t <- alias(y) do
      addAliasTransitive(x, t)
  }


  private def mapLoadToStore(l: Load, s: Store) =
    if !load2Store.contains(l) then
      val fresh = mutable.Set[Store]()
      load2Store += l -> fresh

    if !store2load.contains(s) then
      val fresh = mutable.Set[Load]()
      store2load += s -> fresh

    load2Store(l).add(s)
    store2load(s).add(l)

  override def solve(p: Program, query: Cell): Solution = {
    preprocessAst(p)
    addDemand(query)


    val res = super.naiveSolve(p)
    println("Aliases")
    println(alias)
    println("Alias queries")
    println(a)

    res
  }

  override protected def process(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        if d(x) then
          addToken(x, t)
        if a(x) then
          addAlias(x, x) // reflexive relation
      case Assign(x, y) =>
        if d(x) then
          addDemand(y)
          propagate(x, y)
        if a(y) then // NEEDED?
          addAlias(x, y)

        if a(x) then
          addAlias(x, y)
          addAliasQuery(y)


        // Ensure transitive property for all alias queried variables
        for t <- alias(y) do
          if a(t) then
            addAlias(x, y)

      case l: Load =>
        if d(l.x) then
          // Add alias query for base, and for all store operations over aliases of base, demand src and propagate
          addAliasQuery(l.y)
          for s <- findAlias(l) do
            addDemand(s.y)
            propagate(l.x, s.y)
        if a(l.x) then
          addAliasQuery(l.y)
          for s <- findAlias(l) do
            addAlias(l.x, s.y)

      case s: Store =>
        if a(s.y) then
          addAliasQuery(s.x)
          for l <- findAlias(s) do
            addAlias(s.y, l.x)
  }


  private def findAlias(l: Load): mutable.Set[Store] =
    val res = mutable.Set[Store]()
    for s <- load2Store(l) do
      if alias(s.x).contains(l.y) then
        res.add(s)
    res

  private def findAlias(s: Store): mutable.Set[Load] =
    val res = mutable.Set[Load]()
    for l <- store2load(s) do
      if alias(s.x).contains(l.y) then
        res.add(l)
    res



  override def cost: Int = ???

  private def preprocessAst(p: Program) = {
    p.getInstructions.foreach {
      case a: Load =>
        p.getInstructions.foreach {
          case b: Store => if a.f == b.f then mapLoadToStore(a, b)
          case _ =>
        }
      case _ =>
    }
  }

}
// Program with transitive alias query property required
//x2 = new t0
//x1 = new t1
//x2 = x1
//x0 = new t2
//x0 = x0.f0
//x2.f0 = x2
//x1 = x0

// OTHER
//x0 = new t0
//x2 = new t1
//x0 = x2
//x1 = new t2
//x1 = x0
//x2.f0 = x0
//x2 = x1.f0
