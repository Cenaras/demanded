class CHT(res: SVFResult) extends DemandedSVFSolver(res) {

  override def solve(query: Int): CSolution = {
    addDemand(query)

    while (changed) {
      changed = false
      res.program.getInstructions.foreach(process)
    }

    sol
  }

  override protected def process(i: CInstruction): Unit = {
    i match
      case AddrOf(x, y) =>
        if demanded(x) then addPts(x, y)
        if tracked(y) then
          addDemand(x)
          addPts(x, y)

      case Copy(x, y) =>
        if demanded(x) then
          addDemand(y)
          addTracked(y)
          propagate(y, x)
        for z <- sol(y) do
          if tracked(z) then
            addPts(x, z)
            addTracked(x)

      case CLoad(x, y) =>
        if demanded(x) then
          addDemand(y)
          addTracked(y)
          for z <- sol(y) do
            addDemand(z)
            addTracked(z)
            propagate(z, x)

        for z <- sol(y) do
          for u <- sol(z) do
            if tracked(u) then
              addPts(x, u)
              addTracked(x)


      case CStore(x, y) =>
        for z <- sol(x) do
          if demanded(z) then
            addDemand(y)
            addTracked(y)
            propagate(y, z)

        for u <- sol(y) do
          if tracked(u) then
            addDemand(x)
            addTracked(x)
            for z <- sol(x) do
              addPts(z, u)
              addTracked(z)



        // Ideas: dst ∈ D ⇒ base ∈ D
        // ∀t ∈ base: gepMap(t) ∈ T ⇒ dst ∈ D ???
      case Gep(dst, base, offset) =>
        // TODO: Figure this out
        if demanded(dst) then
          addDemand(base)
          for z <- sol(base) do
            val gepNode = res.gepVarObjMap(z, offset)
            addPts(dst, gepNode)




      // Exhaustive: ∀t ∈ ⟦base⟧: gepMap(t) ∈ ⟦dst⟧
  }
}
