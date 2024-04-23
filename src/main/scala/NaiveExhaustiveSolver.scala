class NaiveExhaustiveSolver extends ExhaustiveSolver {


  override protected def process(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        addToken(x, t)
      case Assign(x, y) =>
        propagate(x, y)
      case Load(x, y, f) =>
        sol(y).foreach(t => {
          propagate(x, (t, f))
        })
      case Store(x, f, y) =>
        sol(x).foreach(t => {
          propagate((t, f), y)
        })
  }

  override def solve(p: Program): Solution = {
    while (changed) {
      changed = false
      p.getInstructions.foreach(i => {
        process(i)
      })
    }
    sol
  }

}
