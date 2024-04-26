import scala.collection.mutable


/** Magic sets implementation in a constraint-based approach. The solver differs from Heintze-Tardieu in a couple of ways.
 *  It stores points-to information in two separate sets, and returns the union of these. The extra solution set is used
 *  to track tokens which are used in store operations. Furthermore, the notion of token tracking is slightly different.
 *  A token is tracked relative to a variable, and intuitively this can be seen as a query asking "if t can reach x".
 *
 *  More specifically, whenever a write operation x.f = y occurs, we track t in x, for all demanded t.f. This intuitively
 *  means, that a read occured over t.f, and any alias might affect this, so we must know if t can reach x.
 *  This information is "propagated backwards" via reverse-assignments, until a new instruction is encountered. Then the
 *  bb-solution set is populated, indicating that this token did indeed reach the write operation. It can then be
 *  propagated forwards.
 *
 * */
class MagicSets extends DemandedSolver {
  val d: mutable.Set[Cell] = mutable.Set[Cell]()

  // Queries representing the question "Can token t reach this constraint variable?".
  val r: mutable.Map[Cell, mutable.Set[Token]] = mutable.Map[Cell, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)

  // Points-to set for tokens used in write operations
  private val sol_bb = mutable.Map[Cell, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)

  protected def addToken_bb(x: Cell, t: Token): Unit = {
    if !sol_bb.contains(x) then
      val fresh = mutable.Set[Token]()
      sol_bb += x -> fresh

    changed |= sol_bb(x).add(t)
  }


  private def addTokens_bb(c: Cell, tokens: mutable.Set[Token]): Unit = {
    for t <- tokens do
      addToken_bb(c, t)
  }

  private def addDemand(c: Cell): Unit = {
    changed |= d.add(c)
  }

  private def addTracking(c: Cell, t: Token): Unit = {
    if !r.contains(c) then
      val fresh = mutable.Set[Token]()
      r += c -> fresh

    changed |= r(c).add(t)
  }

  private def trackAll(c: Cell, tokens: mutable.Set[Token]): Unit = {
    for t <- tokens do
      addTracking(c, t)
  }

  override def process(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        if d(x) then addToken(x, t) // (15)
        if r(x).contains(t) then addToken_bb(x, t) // (12)

      case Assign(x, y) =>
        if (d(x)) {
          addDemand(y) // (6)
          propagate(x, y) // (16)
        }

        trackAll(y, r(x)) // (1)
        addTokens_bb(x, r(x).intersect(sol_bb(y))) // (13)

      case Load(x, y, f) =>
        if (d(x)) {
          addDemand(y) // (7)
          for (t <- sol(y)) {
            addDemand(t, f) // (11)
            propagate(x, (t, f)) // (17)
          }
        }

        if (r(x).nonEmpty) {
          addDemand(y) // (5)
        }

        for (t <- sol(y)) {
          trackAll((t, f), r(x)) // (10)
        }

        for (t <- sol(y)) {
          addTokens_bb(x, r(x).intersect(sol_bb(t, f))) // (14)
        }

      case Store(x, f, y) =>
        for (t <- sol_bb(x)) {
          if (d(t, f)) {
            addDemand(y) // (8)
            propagate((t, f), y) // (19)
          }
        }

        for ((c, v) <- List.from(r)) {
          c match
            case a: Var =>
            case b: (Token, Field) => if b._2 == f then addTracking(x, b._1) // (2)
        }

        for (t <- sol_bb(x)) {
          trackAll(y, r(t, f)) // (3)
        }

        for (c <- d) {
          c match
            case a: Var =>
            case b: (Token, Field) => if b._2 == f then addTracking(x, b._1) // (4)
        }

        for (t <- sol_bb(x)) {
          addTokens_bb((t, f), r(t, f).intersect(sol_bb(y))) // (18)
        }
  }

  override def solve(p: Program, query: Cell): Solution = {
    addDemand(query)

    // output both types of points-to sets
    mergeMaps(naiveSolve(p), sol_bb)
  }

  def collectDemand: String = {
    val builder = new StringBuilder()
    d.foldLeft(builder)((acc, c) => {
      c match
        case a: Var => acc.append(s"x$a\n")
        case b: (Token, Field) => acc.append(s"t${b._1}\tf${b._2}\n")
    })
    val res = builder.toString()
    res.linesIterator.toList.sorted.mkString("\n")

  }

  def collectTracked: String = {
    val builder = new StringBuilder()

    r.keySet.foreach(k => {
      val t = r(k)
      k match
        case a: Var =>
          t.foreach(v => {
            builder.append(s"x$k\tt$v\n")
          })
        case b: (Token, Field) =>
          t.foreach(v => {
            builder.append(s"t${b._1}\tf${b._2}\tt$v\n")
          })
    })

    val res = builder.toString()
    res.linesIterator.toList.filter(p => p != "\n").sorted.mkString("\n")
  }

  private def mergeMaps(m1: mutable.Map[Cell, mutable.Set[Token]], m2: mutable.Map[Cell, mutable.Set[Token]]): mutable.Map[Cell, mutable.Set[Token]] = {
    val seq1 = m1.toSeq
    val seq2 = m2.toSeq

    // Construct merged solution set and obey default values as empty to remain consistent with other implementations.
    val res = mutable.Map[Cell, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)
    seq1.map((c, s) => {
      res += c -> s
    })

    seq2.map((c, s) => {
      if res.contains(c) then
        res(c).addAll(s)
      else
        res += c -> s
    })
    res
  }
}
