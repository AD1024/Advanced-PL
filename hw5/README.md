# Homework 5: Typed Lambda Calculus

## Getting oriented

The starter code includes a working interpreter for STLC, but without a
typechecker.  We have the usual files:
- `lexer.mll`: a lexer for STLC
- `parser.mly`: a parser for STLC
- `syntax.ml`: ASTs and syntax-manipulating functions for STLC
- `hw5.ml`: main loop for a REPL for STLC with abbreviations, but missing a type checker.

Familiarize yourself with the main loop. It repeatedly calls `Parser.main` to
obtain the next REPL binding. It then "executes" that binding by evaluating the
given expression, and possibly binding it to a name. The helper function
`process` uses `step_loop` to repeatedly step the expression until it is a
value. It then prints the value and, if requested, binds it to the provided
nome. The value is actually "normalized" before printing using the provided
`normalize`, which does some additional evaluation "under lambdas". This makes
things easier to read and also gives us an opportunity to use true capture
avoiding substitution.

Also familialize yourself with the provided types and functions in `syntax.ml`.
The syntax of STLC types is provided by the nested module `Ty`, which includes
an unparser for types. Expressions are repsented by the type `expr`, and there
are functions for computing free variables, doing capture-detecting
substitution, finding fresh variable names (broken), checking alpha equivalence,
stepping and normalizing expressions and unparsing expressions.

There are also three files related to testing:
- `test.input`: simple example input to the REPL
- `test.expected`: expected output from `test.input`
- `test_syntax.ml`: additional unit tests for `syntax.ml`, specifically for substitution

You can use `dune runtest` to run all the tests, including both `test.input` and
the unit tests in `test_syntax.ml`. At first, the tests should print some lambda
terms to the terminal, and then exit without any error messages.

As an aside, the structure of the `dune` file is a little different than usual,
because Dune only supports unit testing libraries, not executable programs.  The
`dune` file constructs a library called `hw5lib` that consists of all the ML
files except `test_syntax.ml` and `hw5.ml`, and then these two files depend on
the library. (See the `open Hw5lib` statements at the top of those files.)

## Part 1: Typechecking STLC

- Uncomment the expression `ignore (type_infer [] e_expanded);` inside the
  function `process` in the main loop. The tests will now break, because the
  REPL crashes due to `type_infer` not being implemented.

- Implement `type_infer` by translating the long horizontal lines from lecture
  7/8 that define the type system for STLC. Your function should pattern match
  on the expression and apply the corresponding rule from the long horizontal
  lines. Follow the additional instructions in the starter code inside
  `type_infer`.

- The tests should now pass again, but they don't really test the type system.
  Write a few tests and add them to `test.input`. Be sure to write some tests
  that are supposed to typecheck, and some tests that should *not* typecheck,
  and make sure that correct errors are reported. (The exact error message
  doesn't matter, but the more informative the better!) Use `dune runtest` to
  inspect the output, and when it is correct, use `dune promote` to
  automatically update `test.expected`.

  Note: the starter code can crash in certain situations because the provided
  function `normalize` depends on a correct implementation of capture-avoiding
  substitution, which is *not* part of the starter code. If this happens to you,
  you can either wait to run those tests that cause crashes until you complete
  the next part of the homework, or you can write different tests, or you can
  *temporarily* replace `normalize e'` with `e'` inside of `process` (but put it
  back later, because it will help a lot with debugging).

## Part 2: Capture-avoiding substitution done right

- Capture-avoiding substitution is tricky. The starter code contains an
  implementation of "capture-detecting" substitution, which you had to implement
  yourself on Homework 3, where an exception is thrown if variable capture would
  occur.

- An important building block for capture-avoidance is a function `fresh` that
  finds a variable name that is not otherwise used. The starter code ships with
  a broken implementation of fresh that crashes if the given variable name is
  not *already* fresh.

  Fix the implementation of fresh so that it never crashes, but instead actually
  computes a fresh variable name.

  Hint: your implementation should satisfy the property that the name returned
  by `fresh` is not in the set `avoid`. A simple idea would be to add copies of
  a symbol like `'` to the end of the name until it does not appear in `avoid`.
  Another idea would be to append increasing numbers to the end of the name.

- Add at least one test to `test_syntax.ml` that checks that the returned name
  from `fresh` is not in the set `avoid`. Follow the style of that file when
  adding the test. Take a minute to understand the behavior of `check_equiv`,
  and use it in your test.

- Use `fresh` to fix `subst` so that it is "capture-avoiding" rather than merely
  "capture-detecting". Follow the definition from lecture 7/8, subject to the
  discussion in section 6 and lecture 22.

- Uncomment the three provided unit tests for `subst` in `test_synatx.ml` and
  ensure they pass. Also, ensure that they *fail* if you forget to avoid certain
  relevant sets of variable names. (No need to turn anything in for this bullet
  point.)

- Ensure that you have not commented out the call to `normalize` in the main
  loop's `process` function. Then add a test or two to `test.input` that will
  cause `normalize` to call `subst` in a way that requires capture avoidance.

  For example, the following term

      \x:bool. (\y:bool. \x:bool. y) x

  should normalize to (something alpha equivalent to)

      \x:bool. \x0:bool. x

  since in the original term, the innermost \x:bool binding is never
  used.

- Before moving on to System F, I really encourage you to mess around with
  capture avoiding substitution and convince yourself you got it right. I got it
  wrong the first time I wrote the solution to this assignment!

  For example, your could use AFL to fuzz a property like

      FV(e[e1/x]) = (FV(e) - {x}) \cup (if x \in FV(e) then FV(e1) else \emptyset)

  It's really not enough to just get it right. You need to comfortable and
  confident with why you got it write, because three different substitution
  functions are needed for System F, and at least two of them *must* correctly
  avoid capture in order for the type system to work. (Unlike in STLC, where as
  long as you stick to call-by-value, you can get away without avoiding
  capture. Not true in System F at the type level!)

  Write one sentence here describing what measures you took to get comfortable
  with your strategy for capture avoidance, and to gain confidence that it is
  completely 100% correct.

  **YOUR ANSWER HERE**

## Part 3: System F

We will now extend the REPL to support polymorphism à la System F.

- In `syntax.ml`:
  - add new type forms for type variables (of string) and universal types (which
    bind a type variable represented by a string).
  - add new expression forms for big lambda (which binds a type variable
    represented by a string) and for type application (which takes an expression
    and a type)
- Declare the following new tokens in the parser:
  - type variables (which, like the provided term variables
    (`%token <string> ID`), "contain" a string)
  - big lambda
  - the "for all" symbol for universal types
- Lex these symbols in the lexer, as follows.
  - Type variables are like term variables, except that they must start with an
    uppercase letter. (Note that the lexer already forces expression variables
    to begin with a lowercase letter.)
  - Big lambda should be represented by "/\" in ascii.
  - The "for all" symbol should be represented by the keyword "forall" in ascii.
- Parse the new constructs in the parser.
  - Add a nonterminal for `atomic_ty` that locates an `atomic_ty` by coping this line:

        atomic_ty: l = located(raw_atomic_ty)              { l }

  - Add a case for type application to the `raw_app_expr` nonterminal. Type
    application has the same syntax as expression-level function application:
    you just write the type next to the expression, as in

        e ty

    or for a concrete example:

        (/\A. \x:A. x) (forall B. B)

    To simplify parsing and printing, require that the type being applied is an
    `atomic_ty` and not an arbitrary type. (In other words, a function or
    universal type can only be applied if it is inside parentheses.)

  - Add a case for big lambda in `raw_expr`. Be sure to use your token for type
    variables and *not* expression variables.

  - Make it so that type variables and universal types can be parsed as types.

    Hint: You will need to fix a parsing conflict between universal types and
    function types here. There is more than one way to do it. Please ask for
    help if you don't figure it out in, say, 10 minutes.

- At this point, a bunch of functions will be broken. It's time to patch them up
  bit by bit!

- Implement a function `subst` inside the `Ty` module in `syntax.ml` that
  provides capture-avoiding substitution for types and type variables. It should
  have the signature

      subst : string -> t -> t -> t
      let rec subst from to_ ty = (* ... *)

  just like the expression level substitution function, but for types.

  Note that although this function has the same name as the expression-level
  substitution function, we can refer to it unambiguously as `Ty.subst`.

  Hint: Type variables are bound by universal types. Use the same techniques
  that you used at the term level for lambda binders to handle bound variables.

  Hint: Follow the definition of type substitution in types from page 4 of the
  Homework 5 theory handout.

  Hint: You can reuse the same fresh function. Move it up in the file so that it
  is available inside the `Ty` module.

  Hint: You will need to implement a `free_vars` function inside `Ty` as well.
  Follow the definition of `FV(\tau)` from page 4 of the Homework 5 theory
  handout.

- Implement a function `alpha_equiv` that checks alpha equivalence on
  types.

  Hint: There is more than one way to do it. Probably the easiest option is to
  follow the pattern used for the expression-level alpha equivalence function
  provided in the starter code. Another choice is to use (type-level)
  substitution to rename bound variables properly as you traverse the two types.

- Extend the unparser for types to handle type variables and universal types. If
  you don't want to think too hard, just always surround universal types with
  parentheses. It's also not to hard to figure out how to detect whether
  parentheses are needed for universal types, following the example of function
  types in the starter code's unparser.

- Port the tests in `test_syntax.ml` to be about substituting types instead of
  substituting expressions. Basically this corresponds to replacing `lambda` by
  a universal type, and changing lowercase names to uppercase names.

- Now move on to the expression level. Extend `free_vars` and `subst` to handle
  big lambda and type application.

  Hint: These are easier than you might expect. You don't need to do anything
  fancy or avoid any capture, because type variables and expression variables
  are completely distinct.

  Hint: Follow the definitions of `FV` and substitution from page 4 of the
  Homework 5 theory handout.

- Implement a function `free_tyvars` that returns the set of free type variables
  in an expression.

  Hint: Follow the definition of `FTV(e)` from page 4 of the Homework 5 theory
  handout.

- Implement "substituting for a type variable inside an expression" via a
  function `tysubst` whose OCaml type is

      tysubst: string -> Ty.t -> expr -> expr
      let rec tysubst from to_ e = (* ... *)

  Hint: Follow the definition of substituting a type variable in an expression
  from page 4 of the Homework 5 theory handout.

  Hint: All the "interesting" cases in substituting an expression variable
  inside an expression should become "boring", but there should be a couple
  different cases that become "interesting".

- You do *not* need to extend alpha equivalence on expressions to handle the new
  constructs unless you want to. We only needed alpha equivalence for testing
  purposes, and we're now testing our substitution function at the type level,
  using the type-level alpha equivalence function.

  If you want to extend it, you are welcome to do so for extra credit. I haven't
  tried it, but I believe you will need to keep a separate "expression variable"
  and "type variable" contexts for each expression, and to pass the type
  variable context to the type-level alpha equivalence helper function (which
  must be made available, not a private function).

- Extend `step` with cases for applying a big lambda to a type.

- Extend `normalize` with cases for normalizing under a big lambda (similar to a
  little lambda), and for normalizing a type application (similar to function
  application).

Now move to `hw5.ml`.

- Change the type checker to take a type variable context "delta" in addition to
  the existing expression variable context "gamma". Note that "delta" can just
  be a list of type variable names that are in scope; it does not need to be an
  association list that maps names to something, unlike "gamma", which maps
  expression variables to their types.

- Add cases to `type_infer` for big lambda and type application. Follow the
  rules from Homework 5 theory.

  Hint: In order to satisfy the premise in the rule for big lambda that requires
  "\alpha \not\in \Delta", you will need to use a similar technique to the one
  from the definition of substitution, where you rename the bound (type)
  variable so that it avoids all other type variables in scope.

- Since types now have bound (type) variables, it's no longer correct to compare
  types for equality. Fix the provided `type_check` function by replacing the
  call to (dis)equality (`<>`) with an appropriate use of alpha equivalence.

- Implement a function `kind_check` to check the judgment

      \Delta |- \tau

  from the slides/theory homework.

  Hint: There's more than one way to do it. One is to implement a recursive
  function on the syntax of types, following the long horizontal lines. Another
  way is to use the `Ty.free_vars` function and check that it is a subset of
  `delta`.

- Use `kind_check` inside `type_infer` where it is called for by the typing
  rules (ie, where you see \Delta |- \tau in the rules).

- Fix up any remaining OCaml errors in the main loop (eg, you will need to
  initialize `delta` to the empty list).

- Extend the main loop to print the inferred type instead of just ignoring it.

You will test your System F REPL extensively in the next part. For that, it will
be convenient to also support *type abbreviations* in the REPL. This is optional
but highly recommended.

- (Optional but recommended) Extend the `binding` type in `syntax.ml` with a new
  case for a type abbreviation that takes a string an a type.

- In the parser, add a branch to the `main` nonterminal to parse type
  abbreviations with syntax like

      Nat = forall A. (A -> A) -> A -> A;;

  Use your token for type variables for the left hand side of the abbreviation,
  forcing type abbreviations to start with an uppercase letter.

- In the main loop, add a "dynamic type environment" to the REPL, which maps
  type abbreviation names to their definitions. Add case to the main loop to
  handle the new binding for type abbreviations. In all other cases, implement
  and use a `tysubst_all` function to expand type abbreviations, similar to the
  provided expression-level `subst_all`.

  Hint: Think about whether it matters if you expand type abbreviations first or
  expression abbreviations first. If it does matter, be sure to get it in the
  right order :) (There's more than one way to do it, so it may not matter in
  your implementation, or it might!)

- Make sure

      Nat = forall A. (A -> A) -> A -> A;;

  is accepted by your REPL and that you can then type check the identity
  function on Nats:

      \x:Nat. x;;

  should have inferred type `Nat -> Nat`.

## Part 4: Programming in System F

Complete the following programming problems and place your answers in
`test.input`. Use `dune runtest` to check that you are getting reasonable
looking results, and then use `dune promote` to update `test.expected`.

- Implement the polymorphic identity function and ensure that it type checks
  with the expected type

      forall A. A -> A

  (up to alpha equivalence).

- If you did the optional part above, bind the type abbreviation `Nat = forall
  A. (A -> A) -> A -> A`. Otherwise, wherever you see `Nat` below, be prepared
  to manually expand the abbreviation before typing it in to your REPL!

- Implement `zero : Nat` following the Church encoding.

- Implement `succ : Nat -> Nat` following the Church encoding.

- Bind `one = succ zero`. If `zero` and `succ` are correct, and if normalization
  is working, you should see the Church numeral 1 printed back, ie:

      /\A. \s:A->A. \z:A. s z;;

  (up to alpha equivalence) because normalization computes under lambdas (and
  big lambdas). If normalization is not working, you might see something more
  complicated (but equivalent when applied to arguments). Debug normalization
  until you see the simplified result above.

- Implement `plus : Nat -> Nat -> Nat` by adding type annotations, type
  abstractions, and type applications to the untyped plus function that computes
  the sum of two Church nats.

- Bind `two = plus one one`. If `plus` is correct and normalization is working,
  you should see the Church numeral 2 printed back.

- Implement `times : Nat -> Nat -> Nat` that multiplies Church nats.
  (Hint: annotate the untyped implementation.)

- Test `times` on five or six different small pairs of nats and ensure the
  answers are right.

- Bind `four = times two two` and ensure the answer is right.

- Abbreviate `Bool` to the type of Church booleans. (Or expand manually below.)

- Bind `tru : Bool` and `fls : Bool` using the Church encoding. (We use the
  misspelled names to avoid the built-in booleans in our language!)

- Implement "if then else" for Church booleans as a function called
  `ite : forall A. Bool -> A -> A -> A`.

- Implement a function `is_zero : Nat -> Bool`. Test it on `zero`, `one`, `two`, and `four`.

- Type abbreviations are not powerful enough to support "type operators" like tuples.
  Below, you will have to manually expand "Pair A B" into its Church encoding.

  Write a comment (using the `#` character) in `test.input` that describes the
  Church encoding for a pair of `A` and `B`. This will be useful for copy
  pasting later.

- Implement the constructor `pair : forall A. forall B. A -> B -> Pair A B`.

- Implement `fst : forall A. forall B. Pair A B -> A`.

- Implement `snd : forall A. forall B. Pair A B -> B`.

- Implement the predecessor function on Church nats `pred : Nat -> Nat`.

  Hint: Annotate the untyped implementation from Homework 3.

- Bind `three = pred four` and ensure the answer is right.

- Implement `natrec: forall A. (Nat -> A -> A) -> A -> Nat -> A`.

  Hint: Annotate the untyped implementation from Homework 3.

- Implement `factorial: Nat -> Nat` using `natrec`.

- Test `factorial` on all Church nats up to and including `four`.

- Church encoded lists whose elements have type `A` are represented by the type

      # List A = forall B. (A -> B -> B) -> B -> B

  be prepared to manually expand this abbreviation below.

- Implement `nil : forall A. List A`.

- Implement `cons : forall A. A -> List A -> List A`.

- Implement `map : forall A. forall B. (A -> B) -> List A -> List B`.

- A list of nats *can* be abbreviated

      NatList = (forall B. (Nat -> B -> B) -> B -> B)

  Copy this abbreviation into your test file.

- Implement a function `seq : Nat -> NatList`, which, when given an argument
  `n`, returns the list

      0, 1, ..., n-1

  Hint: There are several ways to do it. One way is to use `natrec`, but you
  will need some other list helper functions like append or reverse. Or you
  can do it directly using the Church nat with the "pair trick" and the
  predecessor function.

- Test `seq` on all nats up to and including `four` and ensure that the answers
  are right. (`seq zero` should return the empty list.)

- Implement a function `length : forall A. List A -> Nat` that computes the
  length of a list.

- Test that `length (seq n) = n` for all nats up to and including four.

- Use `map` and `seq` (and some other stuff that's already defined on nats) to
  give another implementation of factorial:

      factorial_via_seq : Nat -> Nat

- Implement a function `natcase : forall A. (Nat -> A) -> A -> Nat -> A` that
  allows you to do "pattern matching" on a natural number without access to a
  "recursive answer" in the successor case.

  Hint: implement it by calling `natrec` in a way that just ignores the
  recursive answer.

- Implement a "less than or equal to" function on nats

      le : Nat -> Nat -> Bool

  Hint: There are several approaches. One way is to recurse over one argument
  while pattern matching on the other argument.

  Hint: You may need to "generalize the induction hypothesis", depending on how
  you set up the recursive problem.

- Write a function `insert : Nat -> NatList -> NatList` that inserts its first
  argument into its second argument in such a way that if the second argument is
  sorted, then the return value of `insert` is also sorted.

  Hint: Use `le` and recursion over the list.

  Hint: Remind yourself how the inner loop of insertion sort works.

- Write a function `insertion_sort : NatList -> NatList` that sorts its argument.

  Hint: Use `insert` in a "loop".

  Hint: Remind yourself how the outer loop of insertion sort works.

- Implement `append : forall A. List A -> List A -> List A`.

- Implement `reverse : forall A. List A -> List A`.

- Test your insertion sort function on some nontrivial inputs, such as

      append Nat (seq four) (seq four)

  or

      reverse Nat (append Nat (seq four) (seq four))

The OCaml binary tree type

    type 'a tree = Leaf | Branch of 'a tree * 'a * 'a tree

can be represented in System F as the (manually expanded) abbreviation

    Tree A = forall B. B -> (B -> A -> B -> B) -> B

which is just the Church encoding of the above OCaml type.

- Implement a function `leaf : forall A. Tree A` corresponding to the constructor.

- Implement a function

      branch : forall A. Tree A -> A -> Tree A -> Tree A

  corresponding to the constructor.

- Implement a function `size : forall A. Tree A -> Nat` that counts how many
  nodes are in the tree. (You can either count leaves as 1 and branches as 1, or
  you can count leaves as 0 and branches as 1. It doesn't matter for our
  purposes.)

A tree of nats can be represented by the actual abbreviation

      NatTree = forall B. B -> (B -> Nat -> B -> B) -> B;;

- Copy this into your test file or be prepared to expand it manually below.

- Write a function `sum : NatTree -> Nat` that adds up all the elements of a tree.

- (Optional. Not used below, but a good warmup for `treerec`.) Write a function

      listrec : forall A. forall B. (A -> List A -> B -> B) -> B -> List A -> B

  that is kind of like `natrec` in that it gives you access to the "current" list.

  Hint: Use the pair trick.

  Hint: The types get *really* ugly. Proceed systematically!

- Write a function

      treerec : forall A. forall B. B -> (Tree A -> B -> A -> Tree A -> B -> B) -> Tree A -> B

  that is kind of like `natrec` and `listrec`, but for trees.

  Hint: Use the pair trick.

  Hint: The types get *exceptionally* ugly. Proceed systematically!

- Write a function `insert_tree : Nat -> NatTree -> NatTree` that inserts the
  given number into the tree in such a way that if the input tree is a binary
  search tree, then the output tree is a binary search tree.

  Hint: Use `treerec`.

- Write a function `insert_all_tree : NatList -> NatTree` that inserts all the
  elements of the given list into an initially empty binary search tree.

- Write a function `tree_to_list : forall A. Tree A -> List A` that returns an
  *in-order* traversal of the given tree.

  Hint: Use `append`.

- Implement a function `tree_sort : NatList -> NatList` that sorts the given
  list by constructing a binary search tree of all the elements and then
  returning an in-order traversal of that tree.

- Test `treesort` on some interesting input data, such as

      append Nat (seq four) (seq four)

  or

      reverse Nat (append Nat (seq four) (seq four))
