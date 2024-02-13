# Demanded points-to analysis
An implementation of various points-to analysis solvers using a simple 4-statement language.
Programs are generated randomly using the ProgramGenerator, and constraints are generated and solved based on the different solver implementations.
The Exhaustive Solver acts as an oracle, reporting the entire points-to set for all constraint variables in the program.

Current implementations are rather naive as they serve as a sanity check for the demanded analyses. 

The *datalog* module holds datalog code which is currently a work-in-progress.
To run the souffle interpreter over the datalog program, run the following command from the
`src/datalog` directory

```bash
cd src/datalog
souffle -F. -D. exhaustive.dl
```

This will output a file names `pointsTo.csv` which contains all points-to facts from the given programs.
To generate facts for a program, use the `Parser.WriteDatalog` function. An example would be

```scala

@main def run(): Unit = {
  val p = ProgramTemplates.LoadStore
  Parser.WriteDatalog(p)
}
```

Running this main program will generate datalog facts corresponding to the program `LoadStore`.