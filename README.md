# OSR-Diversity
This is the home of `osr-diversity`: a compiler-based automated system capable of transforming a `C` program to realize continuous transfers of control and program state among function variants as they run.

The purpose of this system is to provide dynamic diversity (i.e. the program is kept in a single version, but its executions are diverse) features to an existing `C` program by relying on a technique known as _on-stack replacement_.

The design behind this system is described in the paper [_Principled Composition of Function Variants for Dynamic
Software Diversity and Program Protection_](https://dl.acm.org/doi/abs/10.1145/3551349.3559553) which appeared in the [ACM ASE 2022](https://conf.researchr.org/home/ase-2022) conference.

## Quick tutorial
1. Promote the stack variables in your program using the stack variables promotion pass located in `./src/passes/StackVaraPromotion/`
2. Obtain the IR of the `./src/utils/coin_dice.c` program and link it to the IR of your program with promoted stack variables.
3. Run the OSR Creator pass located in `./src/passes/OSRCreator` on the IR obtained after following steps 1 & 2.
