# BCAS (Basic Computer Algebra System)

This repository contains the source code for a basic Computer Algebra System (CAS) written in Haskell. The CAS allows users to manipulate and simplify equations using a set of predefined rules. The system currently supports addition and subtraction operations on integers, variables, and imaginary numbers.

## Project Structure

The project has the following directory structure:

```
.
├── flake.lock            # Lock file for Nix
├── flake.nix             # Nix file that stores all dependencies
├── makefile              # Makefile for compiling, running, and cleaning the program
├── notes                 # Directory for storing project notes
└── src
    ├── CAS.hs            # Defines the rules applied to the Expression, Equation, and [Equation] types
    ├── EquationSearch.hs # Performs search over all possible function applications and selects the best one
    ├── Main.hs           # Aggregates all other Haskell files and creates the user interface
    ├── Parser.hs         # Contains code to parse user input in prefix notation
    └── Types.hs          # Contains the type definitions for Expression and Equation
```

## How to Run the Program

To compile and run the program, follow the steps below:

1. Make sure you have Nix installed on your system.
2. Clone this repository: git clone https://github.com/alexkireeff/CAS.git
3. Navigate to the project directory: cd CAS
4. Run the following command to download the dependencies: nix develop
5. Compile and Execute the program using: make run
6. You will be prompted with a command-line interface (CLI) to interact with the CAS.

## Example Usage

Here is an example session demonstrating the usage of the CAS:

```
Basic Computer Algebra System
Enter a command (add, remove, display, simplify, exit):
add
Enter the left-hand side of the equation:
- a - b - a b
Enter the right-hand side of the equation:
a
Equation added.
Enter a command (add, remove, display, simplify, exit):
simplify
Simplifying equations...
Equations simplified.
[0] - (a - (b - (a b))) = a
Enter a command (add, remove, display, simplify, exit):
exit
Final Equations:
[0] - (a - (b - (a b))) = a
```

## Current Limitations

The CAS is currently limited to performing addition and subtraction operations on integers, variables, and imaginary numbers. The integer limitation is intentional and greatly simplifies the CAS rules. Division is not implemented due to certain complexities and challenges. For example, if an equation a = 2 * a is given, the current version of the system could mistakenly divide both sides by a, eliminating a and giving the impression that there is no solution. This behavior poses difficulties in handling equations involving division accurately. I hope to expand on division in the future, but it requires careful consideration with how the underlying CAS mechanics work.

Feel free to explore the existing codebase and contribute to enhancing the CAS by extending its capabilities to support division and other advanced algebraic operations.
