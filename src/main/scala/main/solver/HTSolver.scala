package main.solver
import main.constraint.Constraints
import main.program.Program

class HTSolver(program: Program) extends Solver(program: Program) {

  
  
  override def solve(): ConstraintVariables = ???

  override def generateConstraints(): Unit = ???

  override def getConstraints: Constraints = ???
}
