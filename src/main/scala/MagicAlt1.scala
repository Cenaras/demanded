import scala.collection.mutable

// Naive implementation of the Alt1 formulation - does not use smart data structures or efficient implementation
class MagicAlt1 extends DemandedSolver {

  val magicBB: mutable.Set[(Var, Token)] = mutable.Set[(Var, Token)]()
  val magicBBB = mutable.Set[(Token, Field, Token)]()

  val magicBF = mutable.Set[Var]()
  val magicBBF = mutable.Set[(Token, Field)]()

  val magicFB = mutable.Set[Token]()

  val pointsToBB = mutable.Set[(Var, Token)]()
  val pointsToBF = mutable.Set[(Var, Token)]()
  val pointsToFB = mutable.Set[(Var, Token)]()

  val pointsToBBB = mutable.Set[(Token, Field, Token)]()
  val pointsToBBF = mutable.Set[(Token, Field, Token)]()


  private def addMagicBB(x: Var, t: Token): Unit =
    changed |= magicBB.add(x, t)

  private def addMagicBBB(t1: Token, f: Field, t2: Token): Unit =
    changed |= magicBBB.add(t1, f, t2)

  private def addMagicBF(x: Var): Unit =
    changed |= magicBF.add(x)

  private def addMagicBBF(t: Token, f: Field): Unit =
    changed |= magicBBF.add(t, f)

  private def addMagicFB(t: Token): Unit =
    changed |= magicFB.add(t)

  private def addPointsToBB(x: Var, t: Token): Unit =
    changed |= pointsToBB.add(x, t)

  private def addPointsToBF(x: Var, t: Token): Unit =
    changed |= pointsToBF.add(x, t)

  private def addPointsToFB(x: Var, t: Token): Unit =
    changed |= pointsToFB.add(x, t)

  private def addPointsToBBB(t1: Token, f: Field, t2: Token): Unit =
    changed |= pointsToBBB.add(t1, f, t2)

  private def addPointsToBBF(t1: Token, f: Field, t2: Token): Unit =
    changed |= pointsToBBF.add(t1, f, t2)


  override def solve(p: Program, query: Cell): Solution = {
    query match
      case a: Var => addMagicBF(a)
      case b: (Token, Field) => throw new Error("Unsupported query for t.f")

    naiveSolve(p)

    val res = mutable.Map[Cell, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)

    // TODO: Clean up. Ideally make a type and add default methods for this behaviour
    for (v, t) <- pointsToBF do
      if !res.contains(v) then
        val fresh = mutable.Set[Token]()
        res += v -> fresh
      res(v).add(t)

    for (v, t) <- pointsToBB do
      if !res.contains(v) then
        val fresh = mutable.Set[Token]()
        res += v -> fresh
      res(v).add(t)

    for (v, t) <- pointsToFB do
      if !res.contains(v) then
        val fresh = mutable.Set[Token]()
        res += v -> fresh
      res(v).add(t)

    for (t1, f, t2) <- pointsToBBF do
      if !res.contains(t1, f) then
        val fresh = mutable.Set[Token]()
        res += (t1, f) -> fresh
      res(t1, f).add(t2)

    for (t1, f, t2) <- pointsToBBB do
      if !res.contains(t1, f) then
        val fresh = mutable.Set[Token]()
        res += (t1, f) -> fresh
      res(t1, f).add(t2)


    res

  }

  // TODO: List.from for each case?
  override protected def process(i: Instruction): Unit =
    for (t, _, _) <- magicBBB do
      addMagicFB(t) // (12)
    for (t, _) <- magicBBF do
      addMagicFB(t) // (13)

    i match {
      case New(x, t) =>
        for (v, t1) <- magicBB do
          if x == v && t == t1 then addPointsToBB(x, t) // (14)
        for v <- magicBF do
          if x == v then addPointsToBF(x, t) // (15)
        for t1 <- magicFB do
          if t == t1 then addPointsToFB(x, t) // (16)


      case Assign(x, y) =>
        for (v, t) <- magicBB do
          if x == v then addMagicBB(y, t) // (1)
        for t <- magicFB do
          addMagicBB(y, t) // (2)
        for v <- magicBF do
          if x == v then addMagicBF(y) // (3)

        for (v, t) <- magicBB do
          if x == v && pointsToBB(y, t) then addPointsToBB(x, t) // (17)
        for v <- magicBF do
          if x == v then
            for (v1, t1) <- pointsToBF do
              if v1 == y then addPointsToBF(x, t1) // (18)
        for t <- magicFB do
          if pointsToBB(y, t) then addPointsToFB(x, t) // (19)


      case Load(x, y, f) =>
        for (v, _) <- magicBB do
          if v == x then addMagicBF(y) // (4)
        for v <- magicBF do
          if v == x then addMagicBF(y) // (5)
        for _ <- magicFB do
          addMagicBF(y) // (6)
        for (v, t) <- magicBB do
          if v == x then
            for (v1, t1) <- pointsToBF do
              if v1 == y then addMagicBBB(t1, f, t) // (7)
        for t <- magicFB do
          for (v1, t1) <- pointsToBF do
            if v1 == y then addMagicBBB(t1, f, t) // (8)
        for v <- magicBF do
          if v == x then
            for (v1, t1) <- pointsToBF do
              if v1 ==y then addMagicBBF(t1, f) // (9)


        for (v, t) <- magicBB do
          if v == x then
            for (v1, t1) <- pointsToBF do
              if v1 == y && pointsToBBB(t1, f, t) then addPointsToBB(x, t) // (20)
        for v <- magicBF do
          if v == x then
            for (v1, t1) <- pointsToBF do
              if v1 == y then
                for (t2, f2, t3) <- pointsToBBF do
                  if t1 == t2 && f2 == f then
                    addPointsToBF(x, t3) // (21)
        for t <- magicFB do
          for (v1, t1) <- pointsToBF do
            if v1 == y && pointsToBBB(t1, f, t) then
              addPointsToFB(x, t) // (22)

      case Store(x, f, y) =>
        for (t1, f1, t2) <- magicBBB do
          if f1 == f && pointsToFB(x, t1) then addMagicBB(y, t2) // (10)
        for (t1, f1) <- magicBBF do
          if f1 == f && pointsToFB(x, t1) then addMagicBF(y) // (11)

        for (t1, f1, t2) <- magicBBB do
          if f1 == f && pointsToFB(x, t1) && pointsToBB(y, t2) then addPointsToBBB(t1, f, t2) // (23)
        for (t1, f1) <- magicBBF do
          if f1 == f && pointsToFB(x, t1) then
            for (v2, t2) <- pointsToBF do
              if y == v2 then
                addPointsToBBF(t1, f, t2) // (24)
    }

  override def cost: Int = ???
}
