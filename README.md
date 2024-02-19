# Demanded points-to analysis
An implementation of various points-to analysis solvers using a simple 4-statement language.
Programs are generated randomly using the `ProgramGenerator`, and constraints are generated and solved based on the
different solver implementations.
The `Exhaustive Solver` acts as an oracle, reporting the entire points-to set for all constraint variables in the
program.

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

# TODO

Add support for function calls and returns from functions. Consider allowing bodies in functions or always keep as
identity functions
Implement an HT variant that uses "the blue tracking constraint variables", i.e. make duplicates of all constraint
variables (at init solving)
Clean up some of the messy code

# Limitations

- Currently, we use an invariant that object token ids and function token ids cannot overlap
- All functions are hard-coded as identity functions
- Function arguments must be unique from normal variables
    - This is maintained in the program generator by using [0, varNum) for variable id's and [varNum, 2varNum) for
      argument id's
- A similar restriction (and solution) applies to object tokens and function tokens