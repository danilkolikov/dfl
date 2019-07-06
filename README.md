![GitHub](https://img.shields.io/github/license/danilkolikov/dfl.svg)
[![Build Status](https://travis-ci.com/danilkolikov/dfl.svg?branch=master)](https://travis-ci.com/danilkolikov/dfl)

# DFL

A compiler of a **D**ifferentiable **F**unctional **L**anguage.

> I am providing code in the repository to you under an open source license.
> Because this is my personal repository, the license you receive to my code is
> from me and not my employer (Facebook).

## What is it about?

The goal of this project is to explore relation between programs and
structures of neural networks.

There clearly is a correspondence between execution of a program, written in
functional paradigm, and flow of data in a neural network. I've already applied
this idea to design of sophisticated networks (see the [FNN](https://github.com/danilkolikov/fnn)
language), which could learn non-trivial algorithms.

This repository is a next iteration of the research. I'll try to implement a
compiler of a differentiable functional language with a haskell-like syntax.

## How to use it?

Currently the project is under development, but one can check it out, build and
test:

```
stack build     # Build the project
stack test      # Run tests
stack haddock   # Build documentation

stack run my_file.dfl         # Run the compiler
stack run my_file.dfl -- -d   # Run the compiler and produce
                              # debug output for every step of
                              # compilation
```

## License

This project is licensed under the MIT License - see the [LICENSE](./LICENSE) file for details.
