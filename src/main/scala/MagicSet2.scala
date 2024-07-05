import scala.collection.mutable

class MagicSet2 extends MagicSet {
  // So far this seems closest to Heintze-Tardieu

  override def solve(p: Program, q: Var): mutable.Map[Cell, mutable.Set[Token]] = {
    addMagicBF(q) // (9)

    while (changed) {
      changed = false
      for i <- p.getInstructions do
        process(i)
      // (10)
      for (k, v) <- magic_bbb do
        addMagicFB(k._1)

      // (11)
      for v <- magic_bbf do
        addMagicFB(v._1)
    }



    mergeSolutions()

  }

  override def process(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        // (15)
        if magic_bb(x).contains(t) then
          addPtBB(x, t)

        // (18)
        if magic_bf(x) then
          addPtBF(x, t)

        // (21)
        if magic_fb(t) then
          addPtFB(x, t)

      case Assign(x, y) =>
        // (1)
        for t <- magic_bb(x) do
          addMagicBB(y, t)

        // (2)
        for t <- magic_fb do
          addMagicBB(y, t)

        // (5)
        if magic_bf(x) then
          addMagicBF(y)

        // (16)
        for t <- pt_bb(y) do
          if magic_bb(x).contains(t) then
            addPtBB(x, t)

        // (19)
        if magic_bf(x) then
          for t <- pt_bf(y) do
            addPtBF(x, t)

        // (22)
        for t <- pt_bb(y) do
          if magic_fb(t) then
            addPtFB(x, t)


      case Load(x, y, f) =>
        // (4)
        if magic_bb(x).nonEmpty then
          addMagicBF(y)

        // (6)
        if magic_bf(x) then
          addMagicBF(y)

        // (7)
        if magic_fb.nonEmpty then
          addMagicBF(y)

        // (12)
        for t <- pt_bf(y) do
          for v <- magic_bb(x) do
            addMagicBBB(t, f, v)

        // (13)
        for t <- pt_bf(y) do
          for v <- magic_fb do
            addMagicBBB(t, f, v)

        // (14)
        if magic_bf(x) then
          for t <- pt_bf(y) do
            addMagicBBF(t, f)

        // (17)
        for t <- pt_bf(y) do
          for v <- pt_bbb(t, f) do
            if magic_bb(x).contains(v) then
              addPtBB(x, v)

        // (20)
        if magic_bf(x) then
          for t <- pt_bf(y) do
            for v <- pt_bbf(t, f) do
              addPtBF(x, v)

        // (23)
        for t <- pt_bf(y) do
          for v <- pt_bbb(t, f) do
            if magic_fb(v) then
              addPtFB(x, v)

      case Store(x, f, y) =>
        // (3)
        for t <- pt_fb(x) do
          for v <- magic_bbb(t, f) do
            addMagicBB(y, v)

        // (8)
        for t <- pt_fb(x) do
          if magic_bbf(t, f) then
            addMagicBF(y)


        // (24)
        for t <- pt_fb(x) do
          for v <- pt_bb(y) do
            if magic_bbb(t, f).contains(v) then
              addPtBBB(t, f, v)

        // (25)
        for t <- pt_fb(x) do
          if magic_bbf(t, f) then
            for v <- pt_bf(y) do
              addPtBBF(t, f, v)

  }
}
