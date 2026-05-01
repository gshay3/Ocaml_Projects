📘 OCaml Functional Programming Projects Collection

This repository contains a series of OCaml programming assignments focused on functional programming, recursion, data structures, and language implementation.

Each project builds progressively from foundational functional techniques to more advanced topics such as abstract data types, automata theory, and interpreter design.
 <br> <br> <br>
📂 Projects Overview

🔹 Project 1: Functional Utilities & List Processing
A collection of OCaml functions that implement fundamental operations, mathematical computations, and advanced list manipulation using recursion and higher-order functions.

Features:

Implements basic utility functions (absolute value, tuple reversal, parity checks) <br>
Provides mathematical operations (Fibonacci, exponentiation, logarithm, GCF) <br>
Supports list manipulation (reverse, zip, merge, insert, membership checks) <br>
Includes positional list operations (every nth element, every xth element) <br>
Implements tuple-based list transformations (jumping tuples) <br>
Uses higher-order functions for filtering, counting, and deduplication <br>
Demonstrates both recursive and fold-based implementations <br>

Concepts Covered:

Recursion and tail recursion <br>
Pattern matching <br>
Higher-order functions (fold, map-style patterns) <br>
List processing and transformations <br>
Functional problem decomposition <br>
Immutability and declarative programming <br>
 <br> <br>
🔹 Project 2: Functional Database & Tree Operations
A functional database system combined with polymorphic tree operations, showcasing abstract data modeling and recursive data structure manipulation.

Features:

Defines a custom person record type with attributes (name, age, hobbies) <br>
Implements a flexible query system using composable conditions (AND, OR, NOT, IF) <br>
Supports database operations (insert, remove, update, delete, sort, query) <br>
Enables condition-based filtering and transformations <br>
Implements a polymorphic binary tree structure <br>
Provides tree operations (map, mirror, traversals, depth calculation) <br>
Reconstructs trees from traversal orders (pre-order and in-order) <br>
Includes tree trimming and initialization functions <br>

Concepts Covered:

Algebraic data types (ADTs) <br>
Higher-order and predicate-based filtering <br>
Functional database design <br>
Recursive tree structures <br>
Tree traversals and folds <br>
Function composition <br>
Immutable data transformations <br>
 <br> <br>
🔹 Project 3: NFA to DFA Conversion (Automata Theory)
An implementation of nondeterministic finite automata (NFA) with epsilon transitions and conversion to deterministic finite automata (DFA) using subset construction.

Features:

Defines a generic NFA type with states, transitions, and alphabet <br>
Implements transition function (move) for state traversal <br>
Computes epsilon-closure for reachable states <br>
Determines string acceptance by an NFA <br>
Generates DFA states from sets of NFA states <br>
Constructs DFA transitions and accepting states <br>
Performs full NFA → DFA conversion using subset construction <br>

Concepts Covered:

Automata theory (NFA, DFA) <br>
Epsilon transitions and closures <br>
State-space exploration <br>
Subset construction algorithm <br>
Functional representation of state machines <br>
List-based set operations <br>
 <br> <br>
🔹 Project 4: SmallC Interpreter (Lexer, Parser, Evaluator)
A full interpreter for a simplified C-like language (SmallC), including lexical analysis, parsing, and execution.

Features:

Implements a lexer to tokenize input strings into language tokens <br>
Supports integers, booleans, identifiers, and operators <br>
Builds an abstract syntax tree (AST) using recursive descent parsing <br>
Handles operator precedence and associativity <br>
Evaluates expressions (arithmetic, logical, relational) <br>
Executes statements (declaration, assignment, print) <br>
Supports control flow (if, while, for loops) <br>
Maintains an environment for variable storage <br>
Includes runtime error handling (type errors, undeclared variables, divide-by-zero) <br>

Concepts Covered:

Language design and implementation <br>
Lexical analysis (tokenization) <br>
Parsing (recursive descent) <br>
Abstract syntax trees (ASTs) <br>
Interpreter design <br>
Environment modeling and state management <br>
Error handling in functional programs <br>
