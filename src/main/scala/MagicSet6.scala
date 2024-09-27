class MagicSet6 extends MagicSet {

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
            for t <- v do
              if magic_bb(x).contains(t) then
                addMagicBB(y, k._1)

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
              addPtBB(x, v)

        // (17)
        if magic_bf(x) then
          for t <- pt_bb(y) do
            for v <- pt_fbf(t, f) do
              addPtBF(x, v)

      case Store(x, f, y) =>
        // (4)
        if pt_bf(x).nonEmpty then
          for t <- magic_fbb(f) do
            addMagicBB(y, t)

        // (6)
        if magic_fbb(f).nonEmpty then
          addMagicBF(x)

        // (7)
        if magic_fbf(f) then
          addMagicBF(x)

        // (8)
        if pt_bf(x).nonEmpty then
          if magic_fbf(f) then
            addMagicBF(y)
          
        // (18)
        for v1 <- pt_bb(y) do
          for v2 <- magic_fbb(f) do
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
