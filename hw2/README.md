# Homework 2: Programming part

# Part 1: Sudoku solver

In this part, you will write a Sudoku solver by translating the puzzle to a
logical formula that can be solved by Z3. Your program will accept a puzzle
on standard input and write its solution (if it has one) on standard output.

## Dependencies

- Install [Z3](https://github.com/Z3Prover/z3/releases)
- Ensure that if you open a new terminal and type `z3`, you get a message
  like this:

      $ z3
      Error: input file was not specified.
      For usage information: z3 -h

  if you instead get something about z3 not being found, debug your
  installation or ask for help.
- `opam install ppx_inline_test ppx_expect` to get the testing framework.

## Getting oriented

Start by looking at `test.input` and `test.expected`, which contain an example
of the input and output format we will use. A puzzle's "concrete syntax" is a
sequence of integers separated by whitespace. The format begins with a single
number that we will call `k` that describes the size of the puzzle. A standard
puzzle has `k = 3`, but any nonnegative value is possible. Let `n = k * k`, then
`n` is the the height and width of the board. There are `n * n`
(that is, `k * k * k * k`) cells on a board. After `k`, the concrete format
for puzzle input and output consists of `n * n` cells separated by whitespace.
Each cell has a number in the range `1 .. n`, or 0, which represents a blank cell.
0 is only used in input puzzles; solved puzzles do not have blanks.

Now take a look at `sudoku.ml`. Its main binding calls `Puzzle.from_channel` (which
you will implement) to read a puzzle from standard input. Next, it prints
the puzzle using `Puzzle.show` (which you will implement). After that,
it spins up a Z3 process and uses it to print "hello from z3" before exiting.

Next, look at `puzzle/puzzle.mli` and `puzzle/puzzle.ml`. The `.mli` file shows
the types of the functions we want to export for use by the main file.  The
`.ml` file contains some incomplete starter code for parsing a puzzle, and an
empty implementation for printing a puzzle. It also contains an "inline test"
(commented out) that tests whether the module can correctly parse and print a
small example puzzle.

Now look at `z3/z3.mli` and `z3/z3.ml`, which contain a small library to talk to
Z3 over a Unix pipe. You can call `Z3.raw_send` to send Z3 a message, and
`Z3.raw_read_line` to read one line of response from Z3.

We have split this part of the project into separate subdirectories for two
different reasons. `Puzzle` is in its own directory so that we can use inline
expectation tests, which are not supported by Dune when building
an executable directly, only on standalone libraries in their own directory.
`Z3` is in its own directory because we will reuse this module on future
homeworks.


## More precise specification

Now that we have an idea what the files are, let's be more precise about the
possible outputs from your program. There are three possibilities

- The input puzzle has no solution. In that case, print "unsat" and exit. See
  `test-nosol.input` and `test-nosol.expected`.

- The input puzzle has exactly one solution. In that case, print the solution in
  the format described above, and then print the string "puzzle solution is
  unique" on a line by itself.

- The input puzzle has more than one solution. In that case, print one solution,
  followed by the string "found more than one solution!" on a line by itself,
  followed by a second solution.

Your program should be able to translate puzzles with any value of `k` in the
range 2 to 100. However, Z3 might not be able to solve them for large k. That's
ok.

## Walkthrough

At this point, you may be able to complete Part 1 without further direction.
Feel free to give it a shot! Here is a bit more of a walkthrough in case you
get stuck.

- Read a little bit about Sudoku if you're not familiar. Make sure you
  understand what it means for a Sudoku board to be a valid solution.

- Make a plan for expressing the constraints of Sudoku in logic. We recommend
  using integer variables, one per cell of the board. But other encodings
  (even to SAT, without integers!) are possible, and you are free to use them
  if you wish. Write down your planned constraints for the `test-small.input`
  puzzle on a piece of paper. (No need to turn in.)

- Translate your logical constraints to Z3 syntax. Debug your encoding by
  making a `.smt2` file and writing the constraints for `test-small.input`.
  Make sure that Z3 returns "sat" and that the values obtained by `get-value`
  correspond to the expected solution in `test-small.expected`. You may find
  the Z3 `distinct` expression useful in your encoding.

- Design a data structure to represent a Sudoku board in OCaml. There are many
  choices here, but keep it simple and think about what kind of access to the
  data you are going to want. Write your data structure as a type definition
  on the first line of `puzzle.ml`.

- Implement `Puzzle.from_channel` and `Puzzle.show`. Follow the instruction in
  the comment at the bottom of `Puzzle.from_channel`. Uncomment the testing code
  at the bottom of the file, and run `dune runtest` *from inside the directory
  `puzzle`* and ensure it passes. (By running Dune in the subdirectory, you
  avoid running the tests from the parent directary, which you're not ready for
  yet.)

- Write a function in `sudoku.ml` that prints out the constraints you designed
  to standard output. To do this, you will either need to expose your data
  structure type definition in `puzzle.mli` or add functions to the interface
  that allow accessing the data you need. Either approach is fine.

  Make sure your constraint-generating function prints exactly the same
  constraints that you wrote by hand in your `.smt2` file. Also make sure Z3
  still finds "sat" and that the values look reasonable.

- Use `Z3.raw_send` to send your constraints to Z3 instead of standard out.  If
  you need to debug, you can uncomment the line in `z3.ml` inside `raw_send` so
  that you can confirm the correct strings are being sent. Check that Z3
  still finds "sat" and that the values look reasonable. We recommend printing
  the result from "(check-sat)" and from "(get-value (...))" to standard out
  so you can see what's going on.

- Parse the output of the "(check-sat)" and "(get-value (...))" to fill in
  the solved board. You will find it useful to make a fresh board object
  for the solved board, because in the next part, you will need access to
  both the unsolved and solved board.

  For parsing, your life will be easier if you use "(eval ...)" rather than
  `get-value`, since `eval` doesn't print the expression being evaluated
  or any parentheses.

  Once you've got this part working, print the solved board to standard out
  using `Puzzle.show`. Confirm that `test-small.input` and `test.input` pass.

- If "(check-sat)" gives back "unsat", print that to standard out and exit.

- Go back to your piece of paper and figure out how to extend your logical
  constraints to check that a solution is unique. Extend your manual `.smt2`
  file to check that the solution to `test-small.input` is unique.

- Implement your uniqueness check. You may have to refactor your existing
  code to make it more flexible. Try to share as much code as possible
  between the part that solves the puzzle and the uniqueness check.

  If you find that the puzzle is unique, print the required message.

  Otherwise, if the puzzle is not unique, print the required message,
  construct the second solved board, and print it.

- If you haven't uncommented the `runtest` rules in the main `dune` file, do so
  now and ensure they pass.

## What to turn in

Zip up the files you edited, along with the dune files, and upload to gradescope.

- sudoku.ml
- dune
- puzzle/puzzle.mli
- puzzle/puzzle.ml
- puzzle/dune
- z3/z3.ml
- z3/z3.mli
- z3/dune

## Extra credit ideas

- Write a Sudoku solver without using Z3. There are many search strategies you
  can use. For extra extra credit, do some reading about CDCL in modern SAT
  solvers and implement a Sudoku solver "inspired by" CDCL (ie, it should
  learn conflicts and backtrack nonchronologically).

- Implement a mode that automatically generates Sudoku puzzles "from scratch".
  Remember that a Sudoku puzzle should have a unique solution, so your
  uniqueness checker will play an important role. It also might be interesting
  to include features to allow the user to control the generation, in essence
  "designing" a Sudoku puzzle with the help of Z3.

- Run the fuzzer on your Sudoku solver.


# Part 2: IMP interpreter and typechecker

In this part, you will write an interpreter and typechecker for a dialect of IMP.

## Getting oriented

The layout of the `imp` directory is similar to Homework 1. The main file is `imp.ml`,
where there is some starter code to call the parser and print the resulting AST.

Take a look at `test.input`, which contains the concrete syntax of our favorite
program from lecture. We have hard-coded the input to be 4. The expected output
in `test.expected` consists of the final "heap" for the program.

Now look at `syntax.ml`. We have refactored the expression AST so that all
binary operations share one constructor, which is now parametrized by a `binop`
enumeration type. This make things a little nicer when you have many operators
in your language. Unary operators are similarly refactored.

Another big change in `syntax.ml` is the use of the `located` type, which wraps
ASTs in a record that contains source location information, which is useful for
error messages. This makes things a little bit messy, but it's really worth it
when you start actually using your language, because debugging even medium-sized
inputs is really hard if you don't even known what line the problem is. Both
`expr` and `stmt` are defined using a "raw" version, which is then wrapped
in a `located` record. Note that this happens recursively, so that every node
has location information.

Next look at `parser.mly`. There is code to parse all the AST nodes from
`syntax.ml` and to collect location information. Read the comments in this file.

## Specification

This part has a less strict specification than Part 1. Your job is to implement
a programming language capable of expressing our favorite program and all the
constructs of IMP we covered in class. You are permitted to design the concrete
syntax, typechecker, and evaluator as you see fit.  We have provided
`test.input` merely as a guideline from our solution.

You should support at least the following statements:

- skip (already in the starter code)
- assignment
- assertions
- sequential composition (';')
- while loops

You should also support these additional expressions:

- comparisons ('=' and '<'); these can be binops

Just like in Homework 1, you should write a typechecker and an evaluator.

Unlike in Homework 1, this is not a REPL. Rather, you parse the whole program
up front, typecheck it, and evaluate it.

## Walkthrough

You are free to proceed without reading further. We will not grade based on
strict adherence to any particular format, but rather we want to ensure you
understand the ideas. This part is important as a warmup for our homework in
week 4, where we will build a tool for verifying IMP programs using Z3.

In any case, here is a suggested walkthrough.

- The starter code does not contain any evaluator or typechecker. Start by
  porting your code from Homework 1. You will need to adapt to the AST
  refactor that introduces binops.

  Temporarily comment out everything having to do with statements from the parser,
  and temporarily change the start symbol to `expr`.

  Get your main binding working in `imp.ml` so that it can parse and evaluate
  expressions, just like from homework 1.

- Undo your temporary changes to the parser.

- Handle the statements of IMP one by one.

- Start by adding assignments. First, add the AST node to `syntax.ml`. Then
  declare any necessary tokens in the parser and define their regular
  expressions in the lexer. Back in the parser, add the grammar rule and
  construct the AST node.

  In `imp.ml` introduce a typechecker and evaluator for statements. Decide how
  you are going to handle the problem of determining what type a variable has.
  There is more than one way to do it. In our solution, we introduced a new
  keyword into the language `var`, and required the programmer to use that
  keyword the first time a variable was assigned to, so that we could use the
  right-hand side of the assignment to figure out the type. You can do that,
  or any other solution you like.

  Implement typechecking and evaluation for assignment statements and hook up
  your main binding so that you can parse an assignment, typecheck it, evaluate
  it, and print the resulting heap on standard output.

- Next, handle assertions in a similar way: declare the AST node in `syntax.ml`,
  declare any tokens in the parser, lex the tokens, then add the grammar rule
  back in the parser. Finally, support assertions in the typechecker and
  evaluator.  If an assertion fails during evaluation, throw a different
  exception or print a different message than you would if a "runtime type
  error" occurred (like adding 0 to true). Be sure to include source location
  information in your messages!

- Now you can do sequential composition. Follow the pattern of adding the AST
  constructor, lexing, parsing, typechecking and evaluating. This will be the
  first time your typechecker and evaluator for statements becomes recursive.

  You're now in a good position to test your assignment and assertion code by
  writing example programs that assign to variables and then assert things about
  those values. (Before adding sequential composition, you couldn't have a
  program do two things!) Write a few of these tests to make sure everything
  is working so far.

- Finally, handle while loops. Notice how the recursion in the evaluation of a
  while loop is fundamentally different than any of the other recursion we've
  seen so far in this class, because it essentially calls itself on the same
  argument. Ponder this deep mystery of the universe.

## What to turn in

Zip up the files you edited, along with the dune files, and upload to gradescope.

Include your test files so we know what concrete syntax you decided to use.
You will not be graded on your tests.

- dune
- imp.ml
- lexer.mll
- parser.mly
- syntax.ml
- test.input
- test.expected

## Extra credit ideas

- There are a million ways to extend the language. Pick a few from below to try
  or choose your own:

    - if statements
    - comment syntax
    - input/output
    - arrays

- Run the fuzzer on your Sudoku solver.
