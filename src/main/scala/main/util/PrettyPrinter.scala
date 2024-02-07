package main.util

import main.constraint.ConstraintVar
import main.program.Program
import main.solver.Solver

import scala.collection.mutable

object PrettyPrinter {

  def printSolution(solution: mutable.Set[ConstraintVar]): Unit = {
  solution.foreach(f => {
    println(f)
    f.solution.foreach(t => {print(t)})
    println()
  })
  }

   def printProgram(program: Program): Unit = {
    program.foreach(f => println(f.print()))
  }

  def printConstraints(solver: Solver): Unit = {
    val constraints = solver.getConstraints
    println("Token Constraints:")
    constraints.addrConstraints.foreach(f => println(f))
    println()
    println("Copy Constraints:")
    constraints.copyConstraints.foreach(f => println(f))
    println()
    println("Complex Constraints:")
    constraints.complexConstraints.foreach(f => println(f))
  }

}