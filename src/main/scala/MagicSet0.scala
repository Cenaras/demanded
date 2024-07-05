class MagicSet0 extends MagicSet {


  override def process(i: Instruction): Unit = {
    i match
      case New(x, t) =>
        // (12)
        if (magic_bb(x).contains(t)) {
          addPtBB(x, t)
        }

        if (magic_bf(x)) {
          addPtBF(x, t) // (15)
        }


      case Assign(x, y) =>
        for (t <- magic_bb(x)) {
          addMagicBB(y, t) // (1)
        }

        if (magic_bf(x)) {
          addMagicBF(y) // (6)
        }

        // (13)
        for (t <- pt_bb(y)) {
          if magic_bb(x).contains(t) then
            addPtBB(x, t)
        }

        if (magic_bf(x)) {
          for t <- pt_bf(y) do
            addPtBF(x, t) // (16)
        }

      case Load(x, y, f) =>
        // (5)
        if (magic_bb(x).nonEmpty) {
          addMagicBF(y)
        }

        if (magic_bf(x)) {
          addMagicBF(y) // (7)
        }

        for (v <- magic_bb(x)) {
          for (t <- pt_bf(y)) {
            addMagicBBB(t, f, v) // (10)
          }
        }

        if (magic_bf(x)) {
          for (t <- pt_bf(y)) {
            addMagicBBF(t, f) // (11)
          }
        }

        // (14)
        for (t <- pt_bf(y)) {
          for (v <- pt_bbb(t, f)) {
            if (magic_bb(x).contains(v)) {
              addPtBB(x, v)
            }
          }
        }

        if (magic_bf(x)) {
          for (t1 <- pt_bf(y)) {
            for (t2 <- pt_bbf(t1, f)) {
              addPtBF(x, t2) // (17)
            }
          }
        }

      case Store(x, f, y) =>
        // (2)
        for ((k, _) <- magic_bbb) {
          if (f == k._2) {
            addMagicBB(x, k._1)
          }
        }

        // (3)
        for (t <- pt_bb(x)) {
          for (v <- magic_bbb(t, f)) {
            addMagicBB(y, v)
          }
        }

        // (4)
        for (k <- magic_bbf) {
          if (f == k._2) {
            addMagicBB(x, k._1)
          }
        }

        // (8)
        for (t <- pt_bb(x)) {
          if (magic_bbf(t, f)) {
            addMagicBF(y)
          }
        }

        // (18)
        for (t <- pt_bb(x)) {
          for (v <- pt_bb(y)) {
            if (magic_bbb(t, f).contains(v)) {
              addPtBBB(t, f, v)
            }
          }
        }

        // (19)
        for (t <- pt_bb(x)) {
          if (magic_bbf(t, f)) {
            for (v <- pt_bf(y)) {
              addPtBBF(t, f, v)
            }
          }
        }

  }

}
