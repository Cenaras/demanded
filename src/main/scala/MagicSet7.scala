class MagicSet7 extends MagicSet {

  override def solve(p: Program, q: Var): Unit = {
    addMagicBF(q) // (9)
  }

  override def process(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        // (12)
        if magic_bb(x).contains(t) then
          addPtBB(x, t)

        // (15)
        if magic_bf(x) then
          addPtBF(x, t)

      case Assign(x, y) =>
        // (1)
        for t <- magic_bb(x) do
          addMagicBB(y, t)

        // (5)
        if magic_bf(x) then
          addMagicBF(y)

        // (13)
        for t <- pt_bb(y) do
          if magic_bb(x).contains(t) then
            addPtBB(x, t)

        // (16)
        if magic_bf(x) then
          for t <- pt_bf(y) do
            addPtBF(x, t)

      case Load(x, y, f) =>
        // (2)
        for (k, v) <- pt_fbb do
          if k._2 == f then
            for t1 <- v do
              if magic_bb(x).contains(t1) then
                addPtBB(y, k._1)

        // (3)
        if magic_bf(x) then
          for (k, _) <- pt_fbf do
            if k._2 == f then
              addMagicBB(y, k._1)

        // (10)
        for t <- magic_bb(x) do
          addMagicFBB(f, t)

        // (11)
        if magic_bf(x) then
          addMagicFBF(f)

        // (14)
        for t <- pt_bb(y) do
          for v <- pt_fbb(t, f) do
            if magic_bb(x).contains(v) then
              addPtBB(x, t)

        // (17)
        if magic_bf(x) then
          for t <- pt_bb(y) do
            for v <- pt_fbf(t, f) do
              addPtBF(x, v)


      case Store(x, f, y) =>
        // (4)
        for t <- magic_fbb(f) do
          addPtBB(y, t)

        // (6)
        for t <- magic_fbb(f) do
          if pt_bb(y).contains(t) then
            addMagicBF(x)

        // (7)
        if magic_fbf(f) then
          addMagicBF(y)

        // (8)
        if magic_fbf(f) then
          if pt_bf(y).nonEmpty then
            addMagicBF(x)

        // (18)
        for v1 <- magic_fbb(f) do
          for v2 <- pt_bb(y) do
            if v1 == v2 then
              for t <- pt_bf(x) do
                addPtFBB(t, f, v1)

        // (19)
        if magic_fbf(f) then
          for t <- pt_bf(x) do
            for v <- pt_bf(y) do
              addPtFBF(t, f, v)


  }
}
