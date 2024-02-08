package main.solver

import main.constraint.{ConstraintVariables, *}
import main.program.*

import scala.collection.mutable

class ExhaustiveSolver {

   def solve(constraints: Constraints): ConstraintVariables = {
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
    constraints.constraintVars
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
        val cvar = constraints.token2Cvar(t) // TODO: t.f
        changed |= constraints.copyConstraints.add(CopyConstraint(dst, cvar))
      })
      case ForallStoreConstraint(base, field, src) => base.solution.foreach(t => {
        val cvar = constraints.token2Cvar(t) // TODO: t.f
        changed |= constraints.copyConstraints.add(CopyConstraint(cvar, src))
      })
    changed
  }


}
