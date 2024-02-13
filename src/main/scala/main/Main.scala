package main

import main.program.{Parser, ProgramTemplates}

object Main {


  @main def run(): Unit = {
    val p = ProgramTemplates.LoadStore
    Parser.WriteDatalog(p)
  }

}
