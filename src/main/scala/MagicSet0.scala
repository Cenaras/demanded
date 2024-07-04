class MagicSet0 extends MagicSet {


  def solve(p: Program, q: Var): Unit = {

    addMagicBF(q) // (9)

  }

  override def process(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        for t1 <- magic_bb(x) do
          if t == t1 then
            addPtBB(x, t) // (12)

        if magic_bf(x) then
          addPtBF(x, t) // (15)


      case Assign(x, y) =>
        for t <- magic_bb(x) do
          addMagicBB(y, t) // (1)

        if magic_bf(x) then
          addMagicBF(y) // (6)

        // (13)
        for t <- pt_bb(y) do
          if magic_bb(x).contains(t) then
            addPtBB(x, t)

        if magic_bf(x) then
          for t <- pt_bf(y) do
            addPtBF(x, t) // (16)

      case Load(x, y, f) =>
        for _ <- magic_bb(x) do
          addMagicBF(y) // (5)

        if magic_bf(x) then
          addMagicBF(y) // (7)

        for v <- magic_bb(x) do
          for t <- pt_bf(y) do
            addMagicBBB(t, f, v) // (10)

        if magic_bf(x) then
          for t <- pt_bf(y) do
            addMagicBBF(t, f) // (11)

        // (14)
        for t <- pt_bf(y) do
          for v <- pt_bbb(t, f) do
            if magic_bb(x).contains(v) then
              addPtBB(x, t)

        if magic_bf(x) then
          for t1 <- pt_bf(y) do
            for t2 <- pt_bbf(t1, f) do
              addPtBF(x, t2) // (17)

      case Store(x, f, y) =>
        // (2)
        for (k, v) <- magic_bbb do
          if f == k._2 then
            addMagicBB(x, k._1)

        // (3)
        for t <- pt_bb(x) do
          for v <- magic_bbb(t, f) do
            addMagicBB(y, v)

        // (4)
        for k <- magic_bbf do
          if f == k._2 then
            addMagicBB(x, k._1)

        // (8)
        for t <- pt_bb(x) do
          if magic_bbf(t, f) then
            addMagicBF(y)

        // (18)
        for t <- pt_bb(x) do
          for v <- pt_bb(y) do
            if magic_bbb(t, f).contains(v) then
              addPtBBB(t, f, v)

        // (19)
        for t <- pt_bb(x) do
          for v <- pt_bf(y) do
            if magic_bbf(t, f) then
              addPtBBF(t, f, v)

  }

}
