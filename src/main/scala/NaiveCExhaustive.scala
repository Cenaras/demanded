import scala.collection.mutable

// TODO: Generalize into trait similar to the Java solver

// TODO: This computes same result as SVF, but it holds more info, specifically for nodes that have
//  3 in their pointsto set which SVF ignores - figure out why!
//  3 is a DummyObjVar which is probably filtered away by SVF...

class NaiveCExhaustive(SVFResult: SVFResult) {

  val sol = mutable.Map[Cell, mutable.Set[Cell]]().withDefaultValue(mutable.Set.empty)
  var changed = true

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

    // Only add nodes if they are non-dummy nodes
    if SVFResult.dummyNodeIds.contains(y) then
      return

    if !sol.contains(x) then
      val fresh = mutable.Set[Cell]()
      sol += x -> fresh

    changed |= sol(x).add(y)
  }

  def propagate(from: Cell, to: Cell): Unit = {
    sol(from).foreach(c => addPts(to, c))
  }

  def process(i: CInstruction): Unit = {
    i match
      case AddrOf(x, y) =>
        addPts(x, y)
      case Copy(x, y) =>
        propagate(y, x)
      case CLoad(x, y) =>
        for c <- sol(y) do
          propagate(c, x)
      case CStore(x, y) =>
        for c <- sol(x) do
          propagate(y, c)
      case Gep(dst, base, offset) =>
        // Following the SVF implementation (Andersen.cpp#processGep) - ∀t ⟦base⟧ : gepMap(t, offset) ∈ ⟦dst⟧
        for t <- sol(base) do
          t match
            case a: Var =>
              val gepNode = SVFResult.gepVarObjMap.get(a, offset)
              addPts(dst, gepNode)
            case b: Cell =>
              throw new Error("TODO IF THIS CAN HAPPEN")
  }


}
