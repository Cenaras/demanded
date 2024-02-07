import scala.collection.mutable
import scala.collection.mutable.Set

type Solution = mutable.Set[ConstraintVar]

// TODO Refactor to trait and have exhaustive and demanded implementation.
class Solver(program: Program) {

  // Map from a variable/token id to its corresponding constraint variable
  private var id2Cvar: Map[Int, ConstraintVar] = Map();
  private var id2Token: Map[Int, Token] = Map()
  // Map from a token to its (token.field) constraint variable
  private var token2Cvar: Map[Token, ConstraintVar] = Map()

  // The collected constraints, separated into address, copy and complex constraints
  val addrConstraints: mutable.Set[AddrConstraint] = mutable.Set()
  val copyConstraints: mutable.Set[CopyConstraint] = mutable.Set()
  val complexConstraints: mutable.Set[ComplexConstraint] = mutable.Set()

  val constraintVars: Solution = mutable.Set()

  // TODO: Test deduplication of set implementation for duplicate statements

  //  private val solution: Solution = mutable.Map()

  // TODO: Call in constructor
  def genConstraints(): Unit = {

    // Returns a constraint var for the variable. Generates a new if one currently doesn't exist and initializes the solution set to be empty.
    def getOrSet(id: VarId): ConstraintVar = {
      id2Cvar.get(id) match
        case Some(value) => value
        case None =>
          val cvar = new BaseConstraintVar(id)
          id2Cvar += (id, cvar)
          //          solution.update(cvar, mutable.Set())
          constraintVars.add(cvar)

          cvar
    }

    // Generates a token, and a constraint variable for the token with field f
    def getOrSetToken(id: TokenId): Token = {
      id2Token.get(id) match
        case Some(v) => v
        case None =>
          val token = new Token(id)
          id2Token += (id, token)
          val tokenCvar = FieldConstraintVar(token, "f")
          token2Cvar += (token, tokenCvar)
          //          solution.update(tokenCvar, mutable.Set())
          constraintVars.add(tokenCvar)
          token
    }

    // Generate constraints, and initialize the solution set for new constraints
    program.foreach {
      case NewInsn(varId, tokenId) =>
        val cvar = getOrSet(varId)
        val token = getOrSetToken(tokenId)
        addrConstraints += AddrConstraint(cvar, token)

      case AssignInsn(leftId, rightId) =>
        val leftCvar = getOrSet(leftId)
        val rightCvar = getOrSet(rightId)
        copyConstraints += CopyConstraint(leftCvar, rightCvar)

      case LoadInsn(leftId, rightId, field) =>
        val dstCvar = getOrSet(leftId)
        val baseCvar = getOrSet(rightId)
        complexConstraints += ForallLoadConstraint(baseCvar, dstCvar, field)
      case StoreInsn(leftId, field, rightId) =>
        val src = getOrSet(rightId)
        val baseCvar = getOrSet(leftId)
        complexConstraints += ForallStoreConstraint(baseCvar, field, src)
    }
  }

  // Incredibly naive solver, iterates all constraints until a fixed point is reached
  def solve(): mutable.Set[ConstraintVar] = {

    println("Solving constraints")
    //    addrConstraints.foreach(c => {
    //      addToken(c.to, c.token)
    //    })
    addrConstraints.foreach(c => {
      c.to.addToken(c.token)
    })

    var changed = true;
    while (changed) {
      changed = false

      copyConstraints.foreach(c => {
        changed |= propagate(c.from, c.to)
      })


      complexConstraints.foreach(c => {
        changed |= insertEdges(c)
      })

    }
    constraintVars
  }


  //  private def addToken(cVar: ConstraintVar, token: Token): Boolean = {
  //    solution(cVar).add(token)
  //  }

  private def propagate(from: ConstraintVar, to: ConstraintVar): Boolean = {
    var changed = false
    //    val fromTokens = solution(from)
    //    val toTokens = solution(to)
    //    fromTokens.foreach(t => {
    //      changed |= toTokens.add(t)
    //    })

    val fromTokens = from.solution;
    fromTokens.foreach(t =>
      changed |= to.addToken(t))
    changed
  }

  private def insertEdges(constraint: ComplexConstraint): Boolean = {
    var changed = false;
    constraint match
      // TODO: Update such that field is actually used rather than hardcoded to f
      case ForallLoadConstraint(base, dst, field) =>
        //        val solBase: mutable.Set[Token] = solution(base);
        base.solution.foreach(t => {
          val cVar = token2Cvar(t);
          changed |= copyConstraints.add(CopyConstraint(dst, cVar))
        })
      case ForallStoreConstraint(base, field, src) =>
        //        val solBase: mutable.Set[Token] = solution(base);
        base.solution.foreach(t => {
          val cVar = token2Cvar(t);
          changed |= copyConstraints.add(CopyConstraint(cVar, src))
        })
    changed
  }

}


type ComplexConstraint = ForallLoadConstraint | ForallStoreConstraint

sealed trait Constraint {}

case class AddrConstraint(to: ConstraintVar, token: Token) extends Constraint {
  override def equals(obj: Any): Boolean = obj match
    case AddrConstraint(to, token) => true
    case _ => false

  override def hashCode(): Int = (to, token).hashCode()
}


case class CopyConstraint(to: ConstraintVar, from: ConstraintVar) extends Constraint {
  override def equals(obj: Any): Boolean = obj match
    case CopyConstraint(to, from) => true
    case _ => false

  override def hashCode(): Int = (to, from).hashCode()

}


case class ForallLoadConstraint(base: ConstraintVar, dst: ConstraintVar, field: String) extends Constraint {
  override def equals(obj: Any): Boolean = obj match
    case ForallLoadConstraint(base, dst, field) => true
    case _ => false

  override def hashCode(): Int = (base, dst, field).hashCode()
}

case class ForallStoreConstraint(base: ConstraintVar, field: String, src: ConstraintVar) extends Constraint {
  override def equals(obj: Any): Boolean = obj match
    case ForallStoreConstraint(base, field, src) => true
    case _ => false

  override def hashCode(): Int = (base, field, src).hashCode()

}


sealed trait ConstraintVar {
  def string(): String

  def getId: Int

  implicit val base: Boolean;

  val solution: mutable.Set[Token] = mutable.Set()

  def addToken(token: Token): Boolean = {
    solution.add(token)
  }
}

class BaseConstraintVar(id: Int) extends ConstraintVar {
  def string(): String = {
    "⟦x%d⟧:".format(id)
  }

  implicit val base: Boolean = true;


  override def getId: Int = id
}

// TODO: How should this determine id? Right now we might have a clash between t1 and x1
class FieldConstraintVar(token: Token, field: String) extends ConstraintVar {

  implicit val base: Boolean = false

  override def string(): String = {
    "⟦t%d.%s⟧:".format(token.id, field)
  }

  override def getId: Int = token.id
}

class Token(val id: Int) {
  def string(): String = {
    "t%d".format(id)
  }
}
