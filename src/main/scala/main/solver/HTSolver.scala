package main.solver

import main.constraint.{AddrConstraint, BaseConstraintVar, ComplexConstraint, ConstraintVar, Constraints, CopyConstraint, FieldConstraintVar, ForallLoadConstraint, ForallStoreConstraint, Token}
import main.program.{AssignInsn, LoadInsn, NewInsn, Program, StoreInsn}

import scala.collection.mutable

// TODO: Right now this is basically a copy of the original solver. Abstract things away, remove code duplication!
//  Also, the solve signature is different here, so maybe it doesnt make much sense to have an abstraction of solvers
class HTSolver(program: Program) {

  // Map from a variable/token id to its corresponding constraint variable
  private var id2Cvar: Map[Int, ConstraintVar] = Map();
  private var id2Token: Map[Int, Token] = Map()
  // Map from a token to its (token.field) constraint variable
  private var token2Cvar: Map[Token, ConstraintVar] = Map()

  private val constraintVars: ConstraintVariables = mutable.Set()

  val Q: mutable.Set[ConstraintVar] = mutable.Set()
  val W: mutable.Set[Token] = mutable.Set()

  private val DEBUG = false;

  private def addDemand(constraintVar: ConstraintVar): Boolean = {
    if (Q.add(constraintVar)) {
      debug("Adding %s to demand".format(constraintVar))
      return true
    }
    false
  }


  private def addTracking(token: Token): Boolean = {
    if (W.add(token)) {
      debug("Adding %s to tracking".format(token))
      return true
    }
    false

  }

  private def debug(msg: String): Unit = {
    if (DEBUG) then println(msg)
  }


  // TODO: We can only query base constraint variables like this, we need to be able to query tokens as well.
  //  Hot fix is just a boolean flag. If we expand on this project, it might need a major refactoring
  def solve(constraints: Constraints, queryId: Int): ConstraintVariables = {
    id2Cvar.get(queryId) match
      case None => throw Error("Queried variable does not exist")
      case Some(v) => addDemand(v)
    var changed = true;


    // We also must iterate the address constraints in the demanded version
    while (changed) {
      changed = false

      // Address constraints are only processed if the constraint variable is queried or the token is tracked
      constraints.addrConstraints.foreach(c => {
        if (Q.contains(c.to)) {
          changed |= c.to.addToken(c.token)
          if changed then debug("Processing address constraint %s in %s".format(c.token, c.to))

        }

        if (W.contains(c.token)) {
          changed |= c.to.addToken(c.token)
          if changed then debug("Processing tracking in address constraint %s in %s".format(c.token, c.to))
        }
      })

      // Copy constraints
      constraints.copyConstraints.foreach(c => {
        if (Q.contains(c.to)) {
          changed |= addDemand(c.from)
          changed |= propagate(c.from, c.to)
        }
        val tracked = c.from.solution.intersect(W)
        c.to.addTokens(tracked) // TODO: Copy constraint?
      })

      constraints.complexConstraints.foreach(c => changed |= solveComplex(c, constraints))
    }
    constraintVars
  }


  private def solveComplex(constraint: ComplexConstraint, constraints: Constraints): Boolean = {
    var changed = false
    constraint match {
      case ForallLoadConstraint(dst, base, field) =>
        if (Q.contains(dst)) {
          changed |= addDemand(base)
          base.solution.foreach(t => {
            changed |= addTracking(t)
          })

          base.solution.foreach(t => {
            val tf = token2Cvar(t)
            changed |= addDemand(tf)
            changed |= addCopy(dst, tf, constraints)
          })
        }
        base.solution.foreach(t => {
          val token = id2Token(t.id)
          val cvar = token2Cvar(token)
          val tracked = cvar.solution.intersect(W)
          changed |= dst.addTokens(tracked) // TODO: This is actually a subset constraint from t.f to W
        })

      case ForallStoreConstraint(base, field, src) =>
        base.solution.foreach(t => {
          val tf = token2Cvar(t)
          if (Q.contains(tf)) {
            changed |= addDemand(src)
            changed |= addCopy(tf, src, constraints)
            changed |= tf.addTokens(src.solution.intersect(W)) // TODO: This is a subset constraint
            tf.solution.foreach(t => {
              changed |= addTracking(t)
            })
          }
        })

        if (src.solution.intersect(W).nonEmpty) {
          changed |= addDemand(base)
        }
    }

    changed
  }

  def generateConstraints(): Constraints = {
    val constraints = Constraints(mutable.Set(), mutable.Set(), mutable.Set())
    // Iterate all instructions in the program and generate constraints for them
    program.getInstructions.foreach {
      case NewInsn(varId, tokenId) =>
        val cvar = getOrSetCvar(varId)
        val token = getOrSetToken(tokenId)
        constraints.addrConstraints += AddrConstraint(cvar, token)
      case AssignInsn(leftId, rightId) =>
        val left = getOrSetCvar(leftId)
        val right = getOrSetCvar(rightId)
        constraints.copyConstraints += CopyConstraint(left, right)
      case LoadInsn(dstId, baseId, field) =>
        val dst = getOrSetCvar(dstId)
        val base = getOrSetCvar(baseId)
        constraints.complexConstraints += ForallLoadConstraint(dst, base, field)
      case StoreInsn(baseId, field, srcId) =>
        val base = getOrSetCvar(baseId)
        val src = getOrSetCvar(srcId)
        constraints.complexConstraints += ForallStoreConstraint(base, field, src)
    }
    constraints

  }

  private def getOrSetCvar(varId: Int): ConstraintVar = {
    id2Cvar.get(varId) match
      case Some(value) => value
      case None =>
        val cvar = new BaseConstraintVar(varId)
        id2Cvar += (varId, cvar)
        constraintVars.add(cvar)
        cvar
  }

  private def getOrSetToken(tokenId: Int): Token = {
    id2Token.get(tokenId) match
      case Some(value) => value
      case None =>
        val token = new Token(tokenId)
        id2Token += (tokenId, token)
        // TODO: Don't hardcode + token + field maps to cvar
        val tokenCvar = FieldConstraintVar(token, "f")
        token2Cvar += (token, tokenCvar)
        constraintVars.add(tokenCvar)
        token
  }

  private def propagate(from: ConstraintVar, to: ConstraintVar): Boolean = {
    var changed = false
    val fromTokens = from.solution
    fromTokens.foreach(t => {
      changed |= to.addToken(t)
    })
    if changed then debug("Processing propagation from %s to %s".format(from, to))
    changed
  }

  private def addCopy(to: ConstraintVar, from: ConstraintVar, constraints: Constraints): Boolean = {
    if (constraints.copyConstraints.add(CopyConstraint(to, from))) {
      debug("Added copy constraint from %s to %s".format(from, to))
      return true
    }
    false
  }
}