import scala.collection.mutable

// TODO: Generalize into trait similar to the Java solver

// TODO: This computes same result as SVF, but it holds more info, specifically for nodes that have
//  3 in their pointsto set which SVF ignores - figure out why!
//  3 is a DummyObjVar which is probably filtered away by SVF...

class NaiveCExhaustive(SVFResult: SVFResult) {

  val sol = mutable.Map[Cell, mutable.Set[Cell]]().withDefaultValue(mutable.Set.empty)
  var changed = true


  val debugEdges = mutable.Set[(Cell, Cell)]()
  val debugAddrOf = mutable.Set[(Cell, Cell)]()
  val debugLoad = mutable.Set[(Cell, Cell)]()
  val debugStore = mutable.Set[(Cell, Cell)]()
  val debugGep = mutable.Set[(Cell, Int, Cell)]()

  /** Since solving GEP instructions require knowing the mapping that SVF chose, this must be provided. */
  def solve(): CSolution = {
    while (changed) {
      changed = false
      SVFResult.program.getInstructions.foreach(i => {
        process(i)
      })
    }
    sol
  }

  def addPts(x: Cell, y: Cell): Unit = {
    // Dont add dummy - TODO: I dont know if they add these or not or use the nodes from ander.txt only or what...
    //    if SVFResult.dummyNodeIds.contains(y) then
    //      return

    if !sol.contains(x) then
      val fresh = mutable.Set[Cell]()
      sol += x -> fresh

    changed |= sol(x).add(y)
  }

  def propagate(from: Cell, to: Cell): Unit = {
    if !debugEdges(to, from) then
      debugEdges.add(to, from)
      println(s"$from -- Copy --> $to")
    sol(from).foreach(c => addPts(to, c))
  }

  def process(i: CInstruction): Unit = {
    i match
      case AddrOf(x, y) =>
        addPts(x, y)
        if !debugAddrOf(x, y) then
          debugAddrOf.add(x,y)
          println(s"$y -- Addr --> $x")
      case Copy(x, y) =>
        propagate(y, x)
      case CLoad(x, y) =>
        if !debugLoad(x, y) then
          debugLoad.add(x, y)
          println(s"$y -- Load --> $x")
        for c <- sol(y) do
          propagate(c, x)
      case CStore(x, y) =>
        if !debugStore(x, y) then
          debugStore.add(x, y)
          println(s"$y -- Store --> $x")
        for c <- sol(x) do
          propagate(y, c)
      case Gep(dst, base, offset) =>
        if !debugGep(dst, offset, base) then
          debugGep.add(dst, offset, base)
          println(s"$base -- NormalGep($offset) --> $dst")
        // Following the SVF implementation (Andersen.cpp#processGep) - ∀t ⟦base⟧ : gepMap(t, offset) ∈ ⟦dst⟧
        for t <- sol(base) do
          t match
            case a: Var =>
              val gepNode = SVFResult.mapping.gepVarObjMap(a, offset)
              addPts(dst, gepNode)
            case b: Cell =>
              throw new Error("TODO IF THIS CAN HAPPEN")
  }


}
