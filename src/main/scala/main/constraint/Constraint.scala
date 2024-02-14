package main.constraint


import scala.collection.mutable


type ComplexConstraint = ForallLoadConstraint | ForallStoreConstraint

class Constraints(
                   var addrConstraints: mutable.Set[NewConstraint],
                   var copyConstraints: mutable.Set[CopyConstraint],
                   var complexConstraints: mutable.Set[ComplexConstraint],
                   var id2Cvar: mutable.Map[Int, ConstraintVar],
                   var id2Token: mutable.Map[Int, Token],
                   var tf2Cvar: mutable.Map[(Token, String), ConstraintVar],
                   var constraintVars: ConstraintVariables) {

  def this() = {
    this(mutable.Set(), mutable.Set(), mutable.Set(), mutable.Map(), mutable.Map(), mutable.Map(), mutable.Set())
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

/**
 * A constraint variable, either a base constraint for a variable, or a field constraint for (token, field)
 */
trait ConstraintVar {
  override def toString: String

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

  override def toString: String = {
    "⟦x%d⟧".format(id)
  }
}

case class FieldConstraintVar(token: ObjToken, field: String) extends ConstraintVar {

  override def getId: Int = token.id

  override def toString: String = {
    "⟦t%d.%s⟧".format(token.id, field)
  }

  def getField: String = field

}

trait Token(val id: Int) {

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