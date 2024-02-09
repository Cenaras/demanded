# Demanded points-to analysis
An implementation of various points-to analysis solvers using a simple 4-statement language.
Programs are generated randomly using the ProgramGenerator, and constraints are generated and solved based on the different solver implementations.
The Exhaustive Solver acts as an oracle, reporting the entire points-to set for all constraint variables in the program.

Current implementations are rather naive as they serve as a sanity check for the demanded analyses. 