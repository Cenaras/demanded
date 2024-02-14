package main

import main.program.{Parser, ProgramTemplates}

object Main {


  @main def run(): Unit = {
    val p = ProgramTemplates.demandedSimple
    Parser.WriteDatalog(p)
  }

}
