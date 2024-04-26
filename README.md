# Demanded points-to analysis
An implementation of various points-to analysis solvers using a simple 4-statement language.
Programs are generated randomly using the `ProgramGenerator`, and constraints are generated and solved based on the
different solver implementations.
The `NaiveExhaustiveSolver` acts as an oracle, reporting the entire points-to set for all constraint variables in the
program.

Current implementations are rather naive as they serve as a sanity check for the demanded analyses. 
These solvers include:
 - `NaiveExhaustiveSolver`: A naive fixed point solver
 - `Heintze-Tardieu`: Naive implementation of the Heintze-Tardieu demand driven analysis (no worklist, naive fixed point)
 - `FullFS`: Sridharan et. al. formulation of Heintze-Tardieu.
 - `MagicSets`: Constraint based implementation of a pointer analysis transformed by the magic sets transformation

The tests directory contains several test cases for automatic program generation and comparision between points-to set,
demanded sets and tracked tokens.

The *datalog* module holds datalog code of an exhaustive analysis. 
To run the souffle interpreter over the datalog program, run the following command from the
`src/datalog` directory

```bash
cd src/datalog
souffle -F. -D. exhaustive.dl
```

This will output a file names `pointsTo.csv` which contains all points-to facts from the given programs.
To generate facts for a program, use the `DatalogCompiler.compile` function. An example would be

```scala
@main def run(): Unit = {
  val p = ProgramTemplates.LoadStore
  DatalogCompiler.compile(p)
}
```

Running this main program will generate datalog facts corresponding to the program `LoadStore`.
The framework supports automatic magic sets transformation. Given a program and a query, the exhaustive datalog program
is transformed into a magic sets transformed version. Utility functions for interacting with this are also found in the
`DatalogCompiler` class. An example use is the following
```scala
@main def run(): Unit = {
  val p = ProgramTemplates.LoadStore
  val q = 1
  DatalogCompiler.compileAndAnalyze(p, 1)
  DatalogCompiler.solutionToSingleTSV("outfile")
}
```
This will compile the program, transform the analysis to a demanded version optimized for the given query, and output
the computed points-to sets in the output file.