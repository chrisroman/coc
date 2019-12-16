# Calculus of Constructions
Implementation of the [Calculus of Constructions](https://www.sciencedirect.com/science/article/pii/0890540188900053)
by Coquand and Huet.

# Prerequisites
- Ocaml 4.08.1

## Running programs
To simply build the program, run `make`, which will create a file
`_build/default/main.exe`. To test programs, run `make test`.

## TODO
1. Finish the typing rules for application (substitution)
2. Write a function that gets the type of a term that is in normal form (not
described in the paper but should be relatively intuitive)
3. Write the reduction rules ⊳ presented on page 104
    - May need to write the beta reduction rules somewhere (this might
      actually be what we want to implement, rather than ⊳)
