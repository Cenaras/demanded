import scala.collection.mutable

class MagicSets extends DemandedSolver {
  val d: mutable.Set[Cell] = mutable.Set[Cell]()
  val r: mutable.Set[(Cell, Token)] = mutable.Set[(Cell, Token)]()


  private def addDemand(c: Cell): Unit = {
    changed |= d.add(c)
  }

  private def addTracking(c: Cell, t: Token): Unit = {
    changed |= r.add(c, t)
  }

  // TODO: Not optimal in the sense that we might iterate the same points-to set twice in a rule. Implemented such that
  //  it is easy to relate to the rules from the document, rather than for efficiency
  override def process(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        // (15)
        if d(x) then
          addToken(x, t)
        // (12)
        if r(x, t) then
          addToken(x, t)
      case Assign(x, y) =>
        if d(x) then
          addDemand(y) // (6)
          propagate(x, y) // (16)

        for t <- sol(y) do
          if r(x, t) then
            addToken(x, t) // (13)
          if r(x, t) then
            addTracking(y, t) // (1)

      case Load(x, y, f) =>
        if d(x) then
          addDemand(y) // (7)
          for t <- sol(y) do
            addDemand((t, f)) // (11)
            propagate(x, (t, f)) // (17)

        for t <- sol(y) do
          for o <- sol(t, f) do
            if r(x, o) then addToken(x, o) // (14)
          for (c, o) <- List.from(r) do
            addTracking((t, f), o) // (10)
          if r(x, t) then addDemand(y) // (5)

      case Store(x, f, y) =>
        for t <- sol(x) do
          if d(t, f) then
            addDemand(y) // (8)
            propagate((t, f), y) // (19)

        for t <- sol(x) do
          for o <- sol(y) do
            if r((t, f), o) then addToken((t, f), o) // (18)


        for (c, o) <- List.from(r) do
          c match
            case a: Var =>
            case b: (Token, Field) =>
              addTracking(x, b._1) // (2)

        for t <- sol(x) do
          for (c, o) <- List.from(r) do
            c match
              case a: Var =>
              case b: (Token, Field) =>
                if t == b._1 then addTracking(y, o) // (3)

        for c <- List.from(d) do
          c match
            case a: Var =>
            case b: (Token, Field) => addTracking(x, b._1) // (4)

  }

  override def solve(p: Program, query: Cell): Solution = {
    addDemand(query)
    while (changed) {
      changed = false
      p.getInstructions.foreach(i => {
        process(i)
      })
    }
    sol
  }
}
