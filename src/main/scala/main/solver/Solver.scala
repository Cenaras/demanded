package main.solver

import main.constraint.{AddrConstraint, ConstraintVar, Constraints, CopyConstraint, ForallLoadConstraint, ForallStoreConstraint}
import main.program.Program

import scala.collection.mutable

type ConstraintVariables = mutable.Set[ConstraintVar]

// TODO: I would like the general structure to be: Solver takes a set of constraints. 
//  Generate takes a program and produces a set of constraints from that program. 
//  Then we call solve(generateConstraints(program))

trait Solver(val program: Program) {

  def solve(): ConstraintVariables
  
  def generateConstraints(): Unit
  
  def getConstraints: Constraints
  
}
