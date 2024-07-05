class MagicSet1 extends MagicSet {

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

        // (6)
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
        // (5)
        if magic_bb(x).nonEmpty then
          addMagicBF(y)

        // (7)
        if magic_bf(x) then
          addMagicBF(y)

        // (10)
        for t <- pt_bf(y) do
          for v <- pt_bb(x) do
            addMagicBBB(t, f, v)

        // (11)
        if magic_bf(x) then
          for t <- pt_bf(y) do
            addMagicBBF(t, f)

        // (14)
        for t <- pt_bf(y) do
          for v <- pt_bbb(t, f) do
            if magic_bb(x).contains(v) then
              addPtBB(x, v)

        // (17)
        if magic_bf(x) then
          for t <- pt_bf(y) do
            for v <- pt_bbf(t, f) do
              addPtBF(x, v)


      case Store(x, f, y) =>
        // (2)
        for (k, v) <- magic_bbb do
          if k._2 == f then
            for t <- v do
              addMagicBB(y, t)

        // (3)
        for t1 <- pt_bb(y) do
          for (k, v) <- magic_bbb do
            if k._2 == f then
              for t2 <- v do
                if t1 == t2 then
                  addMagicBB(x, k._1)

        // (4)
        if pt_bf(y).nonEmpty then
          for (t, field) <- magic_bbf do
            if f == field then
              addMagicBB(x, t)
        // (8)
        for (_, field) <- magic_bbf do
          if f == field then
            addMagicBF(y)

        // (18)
        for t <- pt_bb(x) do
          for v <- pt_bb(y) do
            if magic_bbb(t,f).contains(v) then
              addPtBBB(t, f, v)

        // (19)
        for t <- pt_bb(x) do
          if magic_bbf(t,f) then
            for v <- pt_bf(y) do
              addPtBBF(t, f, v)

  }
}
