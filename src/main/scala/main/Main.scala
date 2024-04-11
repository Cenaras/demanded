package main

import main.constraint.ConstraintGenerator
import main.program.{Parser, Program, ProgramDistribution, ProgramGenerator}
import main.solver.HTSolver
import main.util.{DatalogCompiler, PrettyPrinter}

object Main {


  @main def run(): Unit = {
    val p: Program = new ProgramGenerator(12, 4, 50, new ProgramDistribution(25, 25, 25, 25)).generate()
    DatalogCompiler.compile(p)
  }

}
