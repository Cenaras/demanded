import scala.collection.mutable

trait MagicSet {

  var pt_bf= mutable.Map[Var, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)
  var pt_bb = mutable.Map[Var, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)
  var pt_fb = mutable.Map[Var, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)

  var pt_bbf= mutable.Map[(Token, Field), mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)
  var pt_bbb = mutable.Map[(Token, Field), mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)

  var pt_fbb = mutable.Map[(Token, Field), mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)
  var pt_fbf = mutable.Map[(Token, Field), mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)
  
  var magic_bf = mutable.Set[Token]()
  var magic_bbf = mutable.Set[(Token, Field)]()
  
  var magic_bb = mutable.Map[Var, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)
  var magic_bbb = mutable.Map[(Token, Field), mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)
  
  var magic_fbb = mutable.Map[Field, mutable.Set[Token]]().withDefaultValue(mutable.Set.empty)
  var magic_fbf = mutable.Set[Field]()
  
  var magic_fb = mutable.Set[Token]()
  
  
  
  private def addMapSet[K, V](m: mutable.Map[K, mutable.Set[V]], k: K, v: V) = {
    if !m.contains(k) then
      val fresh = mutable.Set[V]()
      m += k -> fresh
    
    m(k).add(v)
  }


  def addMagicBF(x: Var) = magic_bf.add(x)
  def addMagicBBF(t: Token, f: Field) = magic_bbf.add(t, f)
  
  def addMagicBB(x: Var, t: Token) = addMapSet(magic_bb, x, t)
  def addMagicBBB(t1: Token, f: Field, t2: Token) = addMapSet(magic_bbb, (t1, f), t2)
  
  def addMagicFBB(f: Field, t: Token) = addMapSet(magic_fbb, f, t)
  def addMagicFBF(f: Field) = magic_fbf.add(f)
  
  def addPtBF(x: Var, t: Token) = addMapSet(pt_bf, x, t)
  def addPtBB(x: Var, t: Token) = addMapSet(pt_bb, x, t)
  
  def addPtBBF(t1: Token, f: Field, t2: Token) = addMapSet(pt_bbf, (t1, f), t2)
  def addPtBBB(t1: Token, f: Field, t2: Token) = addMapSet(pt_bbb, (t1, f), t2)
  
  def addPtFBB(t1: Token, f: Field, t2: Token) = addMapSet(pt_fbb, (t1, f), t2)
  def addPtFBF(t1: Token, f: Field, t2: Token) = addMapSet(pt_fbf, (t1, f), t2)
  
  def addPtFB(x: Var, t: Token) = addMapSet(pt_fb, x, t)
  
  def addMagicFB(t: Token) = magic_fb.add(t)
  
  def solve(p: Program, q: Var): Unit
  
  def process(i: Instruction): Unit
  
  
  
}
