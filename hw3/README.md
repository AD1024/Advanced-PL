# Homework 3: Programming Part

In this homework, you will write an interpreter for the untyped lambda
calculus, and play with some untyped programming.

## Getting acquainted with the starter code

The starter code is very bare bones. We have our usual syntax/lexer/parser/hw
files.  `syntax.ml` defines a single constructor of `raw_expr`, called
`SillyExpr`.  Concrete syntax for this nonsense expression is tokenized and
parsed by the other files, and the main binding in `hw3.ml` parses expressions
from standard input and prints them out using `Syntax.show_expr`.

In other words, there's plenty of work to do :)

## The goal

Build a REPL for untyped lambda calculus with the following grammar

    e ::= x | \x. e | e e

The REPL continually reads an expression, steps it repeatedly until it is a
value, and prints the result.

Your REPL should also support abbreviations of the form

    x = e

which are stored in a dynamic environment. Later expressions can use previous
abbreviations, and your REPL should substitute in their definitions

After building the REPL in Part 1, there are a few questions about using it to
write some interesting programs in the untyped lambda calculus in Part 2.

## Part 1: Building the REPL

You may proceed without following the directions in this part if you wish. Once
your REPL is working (with abbreviations!), proceed to Part 2 to become a user
of your REPL.

What follows is a high-level walkthrough.

### Parsing untyped lambda calculus

- Go into `syntax.ml` and delete the `SillyExpr` constructor. Add constructors
  to `raw_expr` corresponding to the abstract syntax of untyped lambda calculus
  that we covered in lecture, namely variables, lambdas, and function
  applications. Represent variable names using strings.

- We will use the following concrete syntax for lambda expressions:

      \x. x

  (That's the identity function.) The space after the `.` is not required.

  For function application, we will use juxtaposition, just like OCaml, so the
  identity function applied to itself is written like this:

      (\x. x) (\x. x)

  Again, all spaces are optional in the above expression, but sometimes a space
  might be required, eg, to separate two variables right next to each other, as
  in:

      f x

  which is the function in variable `f` applied to the argument in variable `x`,
  which is different from

      fx

  which is just a variable called "fx".

- In `parser.mly`, declare tokens for backslash, dot, variable names, and
  parentheses.

- In `lexer.mll`, give regular expression definitions for these new tokens.
  Support *at least* the following as valid characters in variable names: upper
  and lowercase letters, numerical digits, and underscores.

- Back in `parser.mly`, add rules to parse variables, lambdas, and function
  applications.

- If you run `dune build`, you will get a bunch of parser conflicts from menhir.
  There are several issues that need to be resolved. Unfortunately, menhir's
  built-in precedence and associativity commands are intended for binary
  operators, primarirly, so they are not relevant, and we must find a different
  solution.

  You will need to manually refactor the grammar of expressions to make
  precedence and associativity explicit. For example, in arithmetic expressions
  with plus and times, consider the following grammar:

      e ::= n | e + e | e * e | (e)

  where we'd like to enforce the usual order of operations. Of course, this is
  what menhirs precedence and associativity commands are for (binary ops!), but
  to illustrate how to do it manually, we refactor as follows:

      atomic_expr ::= n | (expr)

      multiplicative_expr ::= atomic_expr | multiplicative_expr * atomic_expr

      expr ::= multiplicative_expr | expr + multiplicative_expr

  Our top-level grammar symbol is called `expr`, whose job is primarily to deal
  with `+`, and to build up smaller pieces from `multiplicative_expr`, which
  deals with `*` and uses `atomic_expr`, which are the "atoms" of the expression
  language, ie, numeric literals or parenthesized expressions. Notice how
  `atomic_expr` allows nested `expr`s *only* inside parentheses. So there is a
  strict ordering between the three grammar symbols. If you think top-down,
  starting from `expr`, you basically collect all the `+` symbols at the top
  level, and then parse whatever is between them using `multiplicative_expr`,
  which works similarly, but with `*`. Finally, `atomic_expr` is responsible for
  nested expressions and literals.

  In general the "loosest binding" operators will be closer to the start symbol,
  and the tightest binding operators will be in the lowest symbol (the one that
  makes a recursive call inside parentheses).

  Associativity is also represented here. For example, in the rule for `+`,
  notice that the left-hand argument is another `expr`, but the right-hand
  argument is a `multiplicative_expr`. This implements left associativity of
  `+`. A similar thing happens with `*`.

- What you need to do is apply this technique to the untyped lambda calculus
  grammar from class:

      e ::= x | \x. e | e e

  The "loosest binding" operator is lambda; next tightest is function
  application, and finally variable names are atomic. Also, function application
  is left associative.

  Refactor your grammar now to implement these precedences and associativities.

  Each symbol in your grammar will need to manage a location. Use the provided
  `located(...)` parametrized grammar rule to easily wrap a grammar symbol in a
  location, as demonstrated by the provided rule for `expr`.

  You may want to do a little reading about parsing if you aren't familiar with
  this technique. You might also find it instructive to play with the arithmetic
  example above.  If you get stuck, don't sink a bunch of time into it. Ping us!
  It's not our intention for you to grind it out, but on the other hand, we
  don't want to write a 10 page instruction manual for parsing the lambda
  calculus.

- `dune build` should now work. Make sure you've fixed all the menhir errors,
  and let us know if you get stuck.

- Run your executable on at least the following tests and visually confirm that
  your parser is returning ASTs that meet the constraints described below.

      Input              Expected result
    ------------------------------------------------------------------------
      \x. x x            should parse to the same AST as \x. (x x)
      \x. x \x. x        syntax error
      (\x. x) (\x. x)    not a syntax error
      \x. x (\x. x)      also not a syntax error
      x x x              should parse to the same AST as (x x) x
      x (x x)            should be a different AST than the previous test!

- You have a parser for the untyped lambda calculus!

### Implementing substitution

Now we're going to work on interpreting the ASTs we've parsed. The first step is
to implement substitution, which we write in slides as e[v/x].

- It will be useful to be able to get the free variables of an expression.
  Implement a function in `syntax.ml`

      free_vars : expr -> StringSet.t

  Hint: Follow the definition on the slides. Use the documentation for Set in
  the stdlib to see what functions are available on the provided `StringSet`
  module.

  Hint: An `expr` has location info, which is not relevant to this function,
  so you should be doing pattern matching on `e.value`, which is a `raw_expr`.

- Test your `free_vars` function on a few inputs by calling it from the main
  binding in `hw3.ml` and printing the result, or by using the inline test
  feature we introduced in hw2.

  It's worth taking a minute to write a function that can convert a
  `StringSet.t` to a string for printing purposes. You'll need this later.

  No need to turn in your tests for this part.

- In `syntax.ml`, implement a function

      subst : (* x *) string -> (* v *) expr -> (* e *) expr -> expr

  For now, you may assume that `v` has no free variables. However, if this
  assumption is violated, your implementation should throw an exception, *not*
  just return bogus results.

  Hint: This assumption allows you to ignore the side condition in the lambda
  case that's described in the Lecture 7 slides definition of substitution.
  However, it does *not* allow you to ignore the side condition that we forgot
  in the slides, and is only listed in the Homework 3 Theory Part... :)

  Hint: Expressions have location information on them now. So when you want to
  construct a new expression, you first construct the corresponding `raw_expr`
  and then decorate it with some location info. But, outside the parser, there's
  no good location info to use. Instead, use the provided function `with_no_loc`
  to convert a `raw_expr` into an `expr` without any location info.

### Implementing step

Now it's time to evaluate expressions.

- In `syntax.ml`, implement a function

      step : expr -> expr option

  which, when given an expression `e`, returns `Some e'` if `e` can step to
  `e'`, or `None` if `e` cannot step.

  Be careful to only perform *one* step of computation. We implement "step star"
  below.

  Hint: Follow the definition of the small-step operational semantics in the
  slides. If you read the long horizontal lines carefully, they tell you exactly
  what to do.

  Hint: Use your substitution function for the beta case.

- In `hw3.ml`, write a function `step_loop` that calls `Syntax.step` repeatedly
  until it returns `None`, and then returns the last expression it found.

- Call `step_loop` from the main binding, and print the result.

- Add a check to the body of the `loop` function inside the main binding to see
  if the user entered an expression with free variables. If so, print a warning
  that includes the names of the free variables in the expression. (Use your
  function from above to convert a `StringSet.t` into a string to print.) After
  printing the warning, proceed to evaluate the expression anyways.

  Also, catch the exception thrown by substitution if the value `v` being
  substituted has free variables, report an error to the user, and continue
  around the main loop.

- Write a few tests to make sure things are working. Substitution is tricky!

- You now have a fully functional interpreter for the untyped lambda calculus!

### Abbreviations in the REPL

In order to write larger programs in lambda calculus, it's useful to be able to
give names to subprograms so that we can reuse them. We do this "outside the
language", in the sense that we will not extend the syntax of expressions at
all. Instead, it will be implemented by the REPL.

- Declare the token `=` in `parser.mly` and lex it in `lexer.mll`.

- Change the `main` grammar rule to have another possibility, which is a binding
  of some variable name to some expression, as in:

      id = \x.x;;

  which binds the name "id" to the expression `\x.x`. You can either choose to
  create an explicit type for bindings, as we've done before, or you can just
  use a tuple; it's up to you.

- In `hw3.ml` introduce a dynamic environment to the main loop. The dynamic
  environment will map strings to expressions, and is used to represent
  abbreviations. Using the dynamic environment, make the following changes to
  the main loop:

  - When the user enters an expression to be evaluated, first substitute all the
    abbreviations in the dynamic environment, before checking for additional
    free variables (and printing a resulting warning if necessary) and calling
    `step_loop`.

  - If the user enters a binding, *step it to a value* and then add a new entry
    in the dynamic environment.

- The details of the implementation are up to you, but you *must* support
  rebinding of previously used abbreviation names to new values, without
  breaking other abbreviations. For example, the following test should behave as
  described below:

      id = \x.x;;
      id = id id;;
      foo = id;;
      id = \x. x x;;
      foo;;  # this last line should evaluate to \x.x (or something \alpha equivalent)

- Convince yourself that your main loop maintains an invariant on the dynamic
  environment that guarantees that if substitution throws an exception about
  free variables, it's the user's fault, and not because some abbreviation
  failed to get substituted.

  No need to turn anything in about this "convincing yourself".

- (Useful for Part 2) Weaken the assumption in your substitution function so
  that it will accept "values" that have free variables, as long as it never
  encounters a lambda expression with a bound variable of the same name as one
  of the free variables in the value.

  Confirm that the following expression now evaluates successfully in your REPL:

      # allowed because the bound variable x is not the same as the value's
      # free variable y
      (\x. x) y;;

## Part 2: Programming in the untyped lambda calculus

Write the following programs in untyped lambda calculus. Once you start writing
programs of any nontrivial size, reading the verbose output from
`Syntax.show_expr` is super burdonsome. We highly recommend doing at least the
first third of the extra credit problem on "unparsing" below, which will give
you somewhat more readable output. The other parts of that problem will improve
the situation even further, but are a bit trickier.

Save all your bindings from the part in a file called `part2.input` and turn it
in with the rest of your implementation. If we ask you to check that an output
of evaluating some expression is correct, include that expression in your input
file in the right place.

- In section, Brendan covered Church numerals, which are lambda terms that can
  be used to encode the natural numbers. The Church numeral corresponding to the
  number n is

      \f. \x. f (f ... (f x))

  where there are n occurences of `f` in the body of the function. Thus the
  number 0 is represented by

      \f. \x. x

  and 1 is

      \f. \x. f x

  Bind these to abbreviations `zero` and `one`.

- Write a function (and bind it to abbreviation `succ`) that takes a Church
  numeral representing the number n, say, as an argument and returns the Church
  numeral that represents n + 1.

- Evaluate `succ one` in the REPL and confirm that it evaluates to the expected
  value. Bind this to `two`.

  Hint: Since we are doing call-by-value, evaluation stops when you get to a
  lambda, but that makes it hard to see what's happening. One trick you can use
  for debugging purposes is to apply your Church numerals to otherwise-unused
  variables. For example, after binding `two` above, you could try

      two SUCC ZERO;;

  which applies it to the free variables `SUCC` and `ZERO`. You should see the
  result

      SUCC (SUCC ZERO)

  (This trick makes critical use of the loosened assumptions on substitution
  that you implemented at the very end of Part 1.)

- Write a function (and bind it to abbreviation `add`) that takes two Church
  numerals and returns their sum.

- Bind `four` to `add two two`, and use the free-variable debugging trick to
  confirm that your value for `four` is correct.

- Write a function (and bind it to `multiply`) that takes two Church numerals and
  returns their product.

- Bind `four_again` to `multiply two two`, and confirm its value is correct
  using the trick.

- We can "Church encode" pairs of things as follows

      # takes two arguments and constructs the pair of them
      pair = \x. \y. \f. f x y;;

      # projects out the first component of a pair
      fst = \p. p (\x. \y. x);;

  Copy these bindings into your file.

- Bind to `snd` a function that projects out the second component of a pair.

- In a comment in your `part2.input` file, right above your definition of `snd`,
  explain informally how this whole encoding of pairs works.

- Use pairs to help you implement a factorial function on Church numerals.  That
  is, a function that takes a Church numeral and returns a Church numeral
  representing its factorial. Bind it to `factorial`.

  Do *not* use the Z combinator for this part. That comes later :)

  Hint: Eventually, the factorial function will want to return a single Church
  numeral. But during the "loop" of the computation, it needs to track two
  values: the product of numbers processed so far, and the number to keep
  processing. So, use a pair to keep both of these around. At the end, project
  out the piece you need. Ponder deeply how this feels like generalizing the
  induction hypothesis...

- Use the free-variable trick to confirm that `factorial (add two one)`
  represents the number 6, and that `factorial four` represents 24.

- Write a function (abbreviated `natrec`) that is similar in spirit to the
  operation of a Church numeral, except that it also passes the numeral itself
  to the function `f`. In other words, whereas a Church numeral has the
  following ML type

      ('a -> 'a) -> 'a -> 'a

  `natrec` should have the following ML type (if we had types in our
  language...):

      natrec : (ChurchNum -> 'a -> 'a) -> 'a -> ChurchNum -> 'a

  The Church numeral passed to `natrec`'s first argument is the predecessor of
  the number of times the first argument has been called, so

      natrec f x 3

  is equivalent to

      f 2 (f 1 (f 0 x))

  Hint: Use the same pairs trick you used for factorial.

  Hint: You'll know if you got this right if you can solve the next
  problem. There's more than one way to do it.

- Use `natrec` to give another implementation for `factorial` that doesn't need
  to (directly) use any pairs. Bind your new implementation to `factorial_again`.

  Test it using inputs `(add two one)` and `four`, using the trick.

- Bind a function `pred` that takes a Church numeral representing n, and returns
  the Church numeral representing n - 1 if n is positive, or 0 if n is 0.

  Hint: Try to do it directly by "calling" n.

  Hint: You can't :) (at least not easily, I think...)

  Hint: Use `natrec` or a similar pair trick to your first implementation of
  `factorial` to finish the job.

- Bind to `three` the result of `pred four` and confirm it is correct using the trick.

- Implement the Church-encoded booleans that Brendan covered in section as
  bindings `true`, `false`, and `ite`.

  Write a few tests using the booleans to make sure things are working.

- Bind to `is_zero` a function that takes a Church numeral and returns a boolean
  indicating whether it is zero or not.

  Test your `is_zero` function on `four` and make sure it returns false, and not
  some lambda-wrapped garbage containing false.

  You can use a modified version of the free-variable trick to force evaluation
  of booleans. For example,

      is_zero four TRUE FALSE

  should evaluate to TRUE.

- Bind to `Z` the Z combinator that Brendan covered in section.

- Use `Z` to give a third implementation of factorial, and bind it to `factorial_yet_again`.

  Hint: You will need several previously defined functions...

  Test your newest implementation of factorial on `three` and `four`.

- This concludes Part 2. Feel free to play around some more, or try the extra
  credit problems below.

## Extra credit

These problems vary greatly in difficulty, and are not sorted. Read through all
of them in case one later in the list sparks your interest!

### Unparsing

"Unparsing" is the problem of converting an AST to a string in such a way that
it can be parsed back to exactly the same tree.

- Implement a simple unparser for expressions. As a first cut, wrap every lambda
  or function application in parentheses to ensure it could be parsed back
  correctly.  Use your unparser in your main loop to improve usability.

- Improve your unparser by eliminating unnecessary parentheses due to
  precedence. This is a bit tricky, but the basic idea is to keep track of the
  precedence of the enclosing context during the recursive tree traversal. For
  example, if the enclosing context has the precedence of a lambda expression,
  and we're now looking at a function application, then we don't need
  parentheses. On the other hand, if a lambda is the left-hand argument to a
  function application, parentheses are required.

  Confirm that the following expressions unparse correctly (to themselves)

      \x. x x
      (\x. x) x

- Improve your unparser by eliminating unnecessary parentheses due to
  associativity of function application. Extend your context tracking to be able
  to tell when you are the left-argument of a function application. Then, if the
  current expression is also a function application, parentheses are not
  required.

  Confirm that the following expressions unparse correctly (to themselves)

      x y z
      x (y z)

### More untyped programming

Play around with the untyped lambda calculus and Church-encoded data. Here are
some ideas.

- Play with Church encoded lists. Implement your favorite list functions, like
  append, map, and filter.

- Implement a less-than function on Church numerals and use it to implement
  insertion sort on Church-encoded lists of Church numerals.

- Figure out how to Church encode the AST type for untyped lambda calculus
  itself. Implement substitution and single-stepping (both using the built-in
  fold that comes with Church encoding). Also implement step_loop (using the Z
  combinator). Hack your REPL to detect Church-encoded ASTs and print them more
  nicely so you can see what the hell you're doing.

### Improving substitution

Substitution is where all the interesting computation actually happens in the
untyped lambda calculus.

- Improve your substitution function to correctly handle the case when the
  "value" v being substituted can have arbitrary free variables. Do this by
  carefully reading the side conditions on the slide/homework, and by thinking
  about how to choose an alternative name for the bound variable in case of a
  conflict.

- Implement multisubstitutions, which we covered in Lecture 9. Use this to
  improve your REPL's handling of the dynamic environment, to substitute it all
  at once. Ensure you still handle rebinding of abbreviations correctly.

### Alternative representations for bound variables

As we mentioned in Lecture 7/8, there are several alternative ways to represent
bound variables in an expression language.

- Read TAPL Chapter 6 and implement a de Bruijn representation of the untyped
  lambda calculus. Implement the de Bruijn shift function and use it to
  implement substitution. Wallow in the misery that is off-by-one errors.

  Ensure your definition of de Bruijn substitution is correct, especially when
  the "value" v has free variables.

- Hack your de Bruijn representation to track a "name hint", and use it to print
  a human-readable representation of your de Bruijn terms. Ensure that the name
  hint is safe to use in the current contex, and "freshen" it if necessary, by
  finding a number to add to the end of the name so that it becomes safe to use
  in the current context. This will make debugging your answer to the previous
  part much easier. (Except that your printer can also have subtle bugs...)

- Explore a pointer-based representation hinted at by the pictures on the
  slides.  There are many different design choices here, and I'm not an
  expert. Experiment and find a way that allows a clean and obviously correct
  implementation of substitution, and report back! I'd love to hear about it.

### Normalizer

Implement an evaluator that also computes under lambda bindings. This will
require either the first part of the "Improving substitution" extra credit
problem, or the use of one of the alternative representations for bound
variables.

For example, your new evaluator/normalizer should be able to compute

    \x. (\y.y) (\z.z)

into

    \x. (\z.z)

(or something \alpha-equivalent)

Use your normalizer to go back and play around with some of the untyped lambda
calculus programs, and bask in the glory of not having to use the free variable
trick to see what's going on :)

### Using a fuzzer for program synthesis

Try to get `afl` to write some lambda calculus programs. For example, you could
try to get it to write a program that loops for ever by changing `step_loop` to
keep track of all the expressions it has seen before, and to throw an exception
if it sees a duplicate.

Alternatively, you could try to teach `afl` something about Church numerals by
changing your main loop to throw an exception if it ever parses an expression
that evaluates to the factorial correctly on the first 4 numbers.

### STLC

This homework was originally going to also cover implementing a typechecker for
STLC, but I ran out of time. We will cover it in week 5 instead. But feel free
to take a crack at it now. If you do it, turn it in as a separate copy of this
folder.

- Add booleans and if/then/else to syntax, lexer, and parser. Update all
  recursive functions to handle the new expressions.

- Add syntax for types: bool and type -> type.

- Parse types and allow the notation

      \x:bool. x

  on lambda expressions.

- Add a typechecker to `hw3.ml`. For now, your typechecker may assume that all
  bound variables have type annotations using the above syntax.
