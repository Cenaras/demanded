class NaiveExhaustiveSolver extends Solver {


  override protected def process(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        addToken(x, t)
      case Assign(left, right) =>
        propagate(left, right)
      case Load(res, base, field) =>
        sol(base).foreach(t => {
          propagate(res, (base, field))
        })
      case Store(base, field, value) =>
        sol(base).foreach(t => {
          propagate((t, field), value)
        })
  }
}
