package main.solver

import main.constraint.*
import main.program.*
import main.solver.{ConstraintVariables, Solver}

import scala.collection.mutable

class ExhaustiveSolver(program: Program) extends Solver(program: Program) {

  // TODO: Make this generic for solvers. I.e. all solvers must generate constraint variables from ids
  
  // Map from a variable/token id to its corresponding constraint variable
  private var id2Cvar: Map[Int, ConstraintVar] = Map();
  private var id2Token: Map[Int, Token] = Map()
  // Map from a token to its (token.field) constraint variable
  private var token2Cvar: Map[Token, ConstraintVar] = Map()


  private val constraintVars: ConstraintVariables = mutable.Set()


  override def solve(constraints: Constraints): ConstraintVariables = {
    constraints.addrConstraints.foreach(c => {
      c.to.addToken(c.token)
    })

    var changed = true

    while (changed) {
      changed = false

      // Process copy constraints
      constraints.copyConstraints.foreach(c => {
        changed |= propagate(c.from, c.to)
      })

      // Process complex constraints, adding new copy constraints
      constraints.complexConstraints.foreach(c => {
        changed |= solveComplex(c, constraints)
      })
    }
    constraintVars
  }


  override def generateConstraints(): Constraints = {
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
    changed
  }

  private def solveComplex(constraint: ComplexConstraint, constraints: Constraints): Boolean = {
    var changed = false
    constraint match
      case ForallLoadConstraint(dst, base, field) => base.solution.foreach(t => {
        val cvar = token2Cvar(t) // TODO: t.f
        changed |= constraints.copyConstraints.add(CopyConstraint(dst, cvar))
      })
      case ForallStoreConstraint(base, field, src) => base.solution.foreach(t => {
        val cvar = token2Cvar(t) // TODO: t.f
        changed |= constraints.copyConstraints.add(CopyConstraint(cvar, src))
      })
    changed
  }


}
