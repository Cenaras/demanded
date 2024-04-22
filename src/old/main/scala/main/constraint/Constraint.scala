package main.constraint


import scala.collection.mutable


type ComplexConstraint = ForallLoadConstraint | ForallStoreConstraint | CallConstraint

/**
 * A class responsible for storing a set of generated constraints as well as related meta information such as mappings from ids to said constraints
 *
 * @param constraints    set of constraints
 * @param id2Cvar        mapping id's to constraint variables
 * @param tf2Cvar        mapping pairs of (token, field) to constraints variables
 * @param funInfo        mapping function tokens to constraint variables for arguments and return node
 * @param constraintVars list of all constraint variables
 */
class ConstraintEnvironment(
                             var constraints: mutable.Set[Constraint],
                             var id2Cvar: mutable.Map[Int, ConstraintVar],
                             var id2Token: mutable.Map[Int, Token],
                             var tf2Cvar: mutable.Map[(Token, String), ConstraintVar],
                             var funInfo: mutable.Map[FunToken, (ConstraintVar, ConstraintVar)],
                             var constraintVars: ConstraintVariables) {

  def this() = {
    this(mutable.Set(), mutable.Map(), mutable.Map(), mutable.Map(), mutable.Map(), mutable.Set())
  }

  def newConstraints: mutable.Set[NewConstraint] = {
    constraints.collect {
      case a: NewConstraint => a
    }
  }

  def copyConstraints: mutable.Set[CopyConstraint] = {
    constraints.collect {
      case a: CopyConstraint => a
    }
  }

  def complexConstraints: mutable.Set[ComplexConstraint] = {
    constraints.collect {
      case a: ForallStoreConstraint => a
      case b: ForallLoadConstraint => b
      case c: CallConstraint => c
    }
  }

}


trait Constraint {}

case class NewConstraint(to: ConstraintVar, token: Token) extends Constraint {
  override def equals(obj: Any): Boolean = obj match
    case NewConstraint(to, token) => true
    case _ => false

  override def hashCode(): Int = (to, token).hashCode()
}

case class CopyConstraint(to: ConstraintVar, from: ConstraintVar) extends Constraint {
  override def equals(obj: Any): Boolean = obj match
    case CopyConstraint(to, from) => true
    case _ => false

  override def hashCode(): Int = (to, from).hashCode()

}

case class ForallLoadConstraint(dst: ConstraintVar, base: ConstraintVar, field: String) extends Constraint {
  override def equals(obj: Any): Boolean = obj match
    case ForallLoadConstraint(dst, base, field) => true
    case _ => false

  override def hashCode(): Int = (dst, base, field).hashCode()
}

case class ForallStoreConstraint(base: ConstraintVar, field: String, src: ConstraintVar) extends Constraint {
  override def equals(obj: Any): Boolean = obj match
    case ForallStoreConstraint(base, field, src) => true
    case _ => false

  override def hashCode(): Int = (base, field, src).hashCode()
}

// TODO: Can this be represented as a load and a store constraint?
case class CallConstraint(res: ConstraintVar, callNode: ConstraintVar, arg: ConstraintVar) extends Constraint {
  override def equals(obj: Any): Boolean = {
    obj match
      case CallConstraint(res, callNode, arg) => true
      case _ => false
  }
}

/**
 * A constraint variable, either a base constraint for a variable, or a field constraint for (token, field)
 */
trait ConstraintVar {
  override def toString: String = "⟦%s⟧".format(name)

  def name: String

  def getId: Int

  val solution: mutable.Set[Token] = mutable.Set()

  def addToken(token: Token): Boolean = {
    solution.add(token)
  }


  def addTokens(tokens: mutable.Set[Token]): Boolean = {
    var added = false
    tokens.foreach(t => {
      added |= addToken(t)
    })
    added
  }

}

case class BaseConstraintVar(id: Int) extends ConstraintVar {

  override def getId: Int = id

  override def name: String = "x%d".format(id)
}

case class TrackedBaseConstraintVar(id: Int, base: BaseConstraintVar) extends ConstraintVar {

  override def getId: Int = id

  override def toString: String = {
    "⟦x%d⟧_tracked".format(id)
  }

  /** The tracked constraint variables must always be a subset of their representatives */
  override def addToken(token: Token): Boolean = {
    var changed = false
    changed |= solution.add(token)
    changed |= base.addToken(token)
    changed
  }

  override def name: String = "x%d_tracked".format(id)
}

case class FieldConstraintVar(token: Token, field: String) extends ConstraintVar {

  override def getId: Int = token.id

  def getToken: Token = token

  def getField: String = field

  override def name: String = "t%d.%s".format(token.id, field)
}

case class TrackedFieldConstraintVar(token: Token, field: String, base: FieldConstraintVar) extends ConstraintVar {
  override def getId: Int = token.id

  def getField: String = field

  /** The tracked constraint variables must always be a subset of their representatives */
  override def addToken(token: Token): Boolean = {
    var changed = false
    changed |= solution.add(token)
    changed |= base.addToken(token)
    changed
  }

  override def name: String =     "t%d.%s_tracked".format(token.id, field)
}


trait Token(val id: Int) {
  def name: String = "t"+id
  
}


case class ObjToken(override val id: Int) extends Token(id: Int) {
  override def toString: String = "t%d".format(id)

  override def equals(obj: Any): Boolean = {
    obj match
      case ObjToken(id) => true
      case _ => false
  }

  override def hashCode(): Int = id.hashCode()

}

case class FunToken(override val id: Int) extends Token(id: Int) {
  override def toString: String = "f%d".format(id)

  override def equals(obj: Any): Boolean = {
    obj match
      case FunToken(id) => true
      case _ => false
  }

}