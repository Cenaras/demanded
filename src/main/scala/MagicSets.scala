import scala.collection.mutable

class MagicSets extends DemandedSolver {
  val d: mutable.Set[Cell] = mutable.Set[Cell]()
  // TODO: Is this just a two-set variant?
  val r: mutable.Set[(Cell, Token)] = mutable.Set[(Cell, Token)]()

  // The Magic Sets formulation has two pointsTo predicates. TODO: Compare with HT to see if we can see similarity (mebe 2set)
  private val sol_bb = mutable.Map[Cell, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)

  protected def addToken_bb(x: Cell, t: Token): Unit = {
    if !sol_bb.contains(x) then
      val fresh = mutable.Set[Token]()
      sol_bb += x -> fresh

    changed = changed | sol_bb(x).add(t)
  }

  private def addDemand(c: Cell): Unit = {
    changed |= d.add(c)
  }

  private def addTracking(c: Cell, t: Token): Unit = {
    changed |= r.add(c, t)
  }

  override def process(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        if d(x) then addToken(x, t) // (15)
        if r(x, t) then addToken_bb(x, t) // (12)

      case Assign(x, y) =>
        if (d(x)) {
          addDemand(y) // (6)
          propagate(x, y) // (16)
        }

        for ((c, t) <- List.from(r)) {
          if c == x then addTracking(y, t) // (1)
        }
        for (t <- sol(y)) {
          if r(x, t) then addToken_bb(x, t) // (13)
        }

      case Load(x, y, f) =>
        if (d(x)) {
          addDemand(y) // (7)
          for (t <- sol(y)) {
            addDemand(t, f) // (11)
            propagate(x, (t, f)) // (17)
          }
        }

        for ((c, t) <- List.from(r)) {
          if x == c then addDemand(y) // (5)
        }
        for (t <- sol(y)) {
          for ((c, v) <- List.from(r)) {
            if x == c then addTracking((t, f), v) // (10)
          }
        }
        for (t <- sol(y)) {
          for (v <- sol_bb(t, f)) {
            if r(x, v) then addToken_bb(x, v) // (14)
          }
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
          for ((c, v) <- List.from(r)) {
            c match
              case a: Var =>
              case b: (Token, Field) => if t == b._1 && f == b._2 then addTracking(y, v) // (3)
          }
        }

        for (c <- d) {
          c match
            case a: Var =>
            case b: (Token, Field) => if b._2 == f then addTracking(x, b._1) // (4)
        }

        for (t <- sol_bb(x)) {
          for (v <- sol_bb(y)) {
            if r((t, f), v) then addToken_bb((t, f), v) // (18)
          }
        }
  }

  override def solve(p: Program, query: Cell): Solution = {
    addDemand(query)
    while (changed) {
      changed = false
      p.getInstructions.foreach(i => {
        process(i)
      })
    }

    // output both types of points-to sets
    mergeMaps(sol, sol_bb)
  }

  def collectDemand: String = {
    val builder = new StringBuilder()
    d.foldLeft(builder)((acc, c) => {
      c match
        case a: Var => acc.append(s"x$a\n")
        case b: (Token, Field) => acc.append(s"t${b._1}\tf${b._2}")
    })
    val res = builder.toString()
    res.linesIterator.toList.sorted.mkString("\n")

  }

  def collectTracked: String = {
    val builder = new StringBuilder()
    r.foreach((c, t) => {
      c match
        case a: Var => builder.append(s"x$a\tt$t\n")
        case b: (Token, Field) => builder.append(s"t${b._1}\tf${b._2}\tt$t\n")
    })

    val res = builder.toString()
    res.linesIterator.toList.sorted.mkString("\n")
  }

  private def mergeMaps(m1: mutable.Map[Cell, mutable.Set[Token]], m2: mutable.Map[Cell, mutable.Set[Token]]): mutable.Map[Cell, mutable.Set[Token]] = {
    val seq1 = m1.toSeq
    val seq2 = m2.toSeq

    val res = mutable.Map[Cell, mutable.Set[Token]]()
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
