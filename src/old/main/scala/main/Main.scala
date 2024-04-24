package main

import main.constraint.ConstraintGenerator
import main.program.{Parser, Program, ProgramDistribution, ProgramGenerator, ProgramTemplates}
import main.solver.{ExhaustiveSolver, HTSolver}
import main.util.{DatalogCompiler, PrettyPrinter}

object Main {


  @main def run(): Unit = {
//    val p: Program = new ProgramGenerator(12, 4, 50, new ProgramDistribution(25, 25, 25, 25)).generate()
//    DatalogCompiler.compile(p)

    val p: Program = ProgramTemplates.FunCall
    val str = PrettyPrinter.stringifyProgram(p)
    println(str)

    val c = ConstraintGenerator.generate(p)
    val sol = new ExhaustiveSolver().solve(c)
    println(PrettyPrinter.stringifySolution(sol))

  }

}
