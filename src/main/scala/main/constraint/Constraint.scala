package main.constraint

import scala.collection.mutable


type ComplexConstraint = ForallLoadConstraint | ForallStoreConstraint

class Constraints(var addrConstraints: mutable.Set[AddrConstraint], var copyConstraints: mutable.Set[CopyConstraint], var complexConstraints: mutable.Set[ComplexConstraint]) {}

trait Constraint {}

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
  val base: Boolean // TODO: Nicer solution - case class?

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

class BaseConstraintVar(id: Int) extends ConstraintVar {

  override def getId: Int = id;
  override val base: Boolean = true

  override def toString: String = {
    "⟦x%d⟧:".format(id)
  }
}

class FieldConstraintVar(token: Token, field: String) extends ConstraintVar {

  override def getId: Int = token.id

  override val base: Boolean = false

  override def toString: String = {
    "⟦t%d.%s⟧:".format(token.id, field)
  }
}


class Token(val id: Int) {
  override def toString: String = "t%d".format(id)
}