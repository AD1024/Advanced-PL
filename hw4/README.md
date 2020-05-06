# Homework 4: Programming Part

In this homework, you will implement Hoare logic for the IMP language.  You have
two options about how to proceed: you can either start from your solution to
hw2, or you can use the given starter code. In either case, the goal is to be
able to verify IMP programs using Hoare logic.  If you choose to start from hw2,
the details of the syntax are up to you.  In any case, you will be graded on the
correctness of your implementation of wp and your use of z3, not on the details
of how you choose to represent programs.

If you choose to start from hw2, it's probably easier to make a copy of your hw2
directory and to completely ignore the starter code.  Skip to part 2 once you
have implemented verification. The only caveat is that you need to implement
assume statements, which we did not cover in class. Search for
"NOTE(assume-statements)" below and read that part.

## Part 1: Implementing Hoare logic

### Getting acquainted with the starter code

The starter code is a lot bigger this week, partially to try to make hw4 less
work, since it is coming out so late, and partially because we would rather you
spend your energy on verification rather than parsing/typechecking/evaluating.

We have the usual code architecture. The lexer/parser/syntax support statements
and expressions, with a few extensions over hw2. There are many more support
binary operations, including comparisons and boolean implication. For
statements, there is a new `assume` statement, described below, as well as an if
statement. We have also changed the syntax of while loops to include invariant
annotations, using the `invariant` keyword.

Our favorite program is given in `test.input`, and includes an example of an
assume statement and a few loop invariants.

`hw4.ml` contains an interpreter and typechecker for the starter code, as well
as code to parse command line arguments. The CLI to `hw4.ml` consists of two
"modes": interpretation mode and verification mode. Each mode also requires the
`-heap` flag, which specifies the initial values in the heap for any "input"
variables (such as `n` in our favorite program).

When passing command-line arguments via dune, you need to be careful to use `--`
to separate the dune command line from the program's command line, like this:

    dune exec ./hw4.exe -- -interpret -heap n=10 test.input
    
which should print

    [("n", (Syntax.VInt 10))]
    [("y", (Syntax.VInt 0)); ("x", (Syntax.VInt 10)); ("n", (Syntax.VInt 10))]

showing the initial and final heaps. In general, the syntax of the `-heap`
argument is a list of assignments, separated by commas, where an assignment
consists of a variable name followed by an equals sign, followed by an integer
or boolean value, something like this:

    -heap foo=3,bar=false,baz=17
    
All of this is parsed for you an exported by the `Args.parse_args` function in a
record of type `Args.t`.

The main binding of `hw4.ml` uses `parse_args` to get the mode and heap. It uses
the heap to construct an initial heap typing, typechecks the program, and then
matches on what mode it's in. Mode `interpret` is implemented for you, and just
calls the evaluator on the initial heap, printing the resulting final heap.

Your job is to implement verification mode. First, an interlude about assume
statements.

### Assume statements

NOTE(assume-statements): There is one kind of IMP statement that will be useful
for expressing preconditions of programs. The statement `assume e` has somewhat
strange operational semantics. It steps to a special kind of error state,
perhaps called `assumption_violated`, which is then stuck. The idea is that in
theorems about the Hoare logic, we always assume (!) that execution never
reaches this state. So, the Hoare logic does *not* need to rule out these
violations, in contrast to assertions. You should handle assume statements as
follows:

  - During evaluation: evaluate the expression. If it is true, do nothing and
    return the current heap. Otherwise, if it is false, print a warning that an
    assumption was violated, and then return the current heap. (This part is
    done for you in the starter code.)

  - During verification: implement the following rule for computing `wp` for an
    `assume` statement.

        wp(assume e, Q) = (e ==> Q, \emptyset)

    where `==>` is implication. This captures the idea that in order to prove
    `Q` will be true after an assume statement, it suffices to "assume" (!) `e`
    is true now, and then prove `Q` is true now.
    
End of NOTE(assume-statements).

### Implementing verification mode

Here is a walkthrough.

- You will need your z3 library from hw2. Copy it into the `z3/` subdirectory
  and be sure to turn it in with your submission.

- You are going to implement an OCaml function

      wp : (* s *) Syntax.stmt -> (* Q *) Syntax.expr -> Syntax.expr * Syntax.expr list

  that takes a statement `s` and a postcondition `Q` and returns `(P, C)`, where
  P is its weakest precondition and `C` is a set (list) of side conditions.

  Keep the following in mind:

    - Look at the slides for lecture 12 to help you.

    - In order to implement the assignment case of `wp`, you'll need to
      implement substitution on expressions. Since no expressions bind
      variables, this is relatively straightforward, compared to lambda
      calculus.

    - We didn't cover wp of if statements in class. Use the following rule.

        wp(if e { s1 } else { s2 }, Q) = 
          let (P1, C1) = wp(s1, Q)
          let (P2, C2) = wp(s2, Q)
          ((e ==> P1) && (!e ==> P2), C1 \cup C2)

    - For the while statement, remember that the `invariant` keyword can appear
      multiple times, just like in Dafny. "The" loop invariant is obtained by
      and-ing all of these expressions together.

To check correctness of a whole program, we compute its `wp`, and then we want
to check that both the weakest precondition and all the side conditions are
valid using Z3. For that, we need a translator.

- Write a function

      z3_of_expr : expr -> string

  that transforms an IMP expression into a string that represents that
  expression in Z3's SMT input syntax.
  
  Keep the following in mind.

    - Variables translate to themselves. Literals translate to a string
      representing their value (use the OCaml standard library).

    - Unary operators should translate to a string of the form "(op arg)" 
      where "op" is the Z3 symbol representing the operation and "arg" is 
      an SMT syntax string representing the recursive translation of the argument.

    - Binary operators are similar. 
    
    - All the operators in the starter code have a direct translation to a Z3
      operator. (See the documentation linked from the course webpage under Z3,
      especially the Core theory and Int theory links.)
      
      EXCEPT: `!=` (not equal) does not have a direct translation to an
      operator. You will need to expand it to `!(... = ...)` and then translate
      that.

- In the main binding of `hw4.ml`, find the TODO under the Verify branch of the
  `match` on the mode. Delete what's there and verify the program, as follows.

    - Compute `wp` of the input program, call it `(p, cs)`.
    
    - For `p` and every element of `cs`, use Z3 to prove the expression valid,
      as follows:

        - compute the variables used in the expression and declare them as Z3
          constants with the type computed by the starter code in `sigma2`.

        - assert the negation of the translation of the expression
        
        - check whether it is satisfiable
        
        - if it is satisfiable, print the value of each variable in the counter
          example and report the verification error to the user. 
          
        - if it unsatisfiable, silently succeed.
        
    - You may find the `(push)` and `(pop)` commands helpful to manage the
      solver. In particular, it is important that each expression is proved
      valid *indpendently*, so you cannot just keep asserting things without
      properly popping the previous assertion off.
      
    - You will make your life easier for Part 2 by making your error messages as
      helpful as possible. Include the counterexample from Z3, and, if possible
      also a brief description of "why" `wp` wanted this fact to be true. (For
      example, "because the loop invariant should be true on entry to the loop".)

Debug your code by trying to prove the provided `test.input` program (our
favorite!) correct. Also check that if you delete any of the provided
invariants, your verification tool reports an error.

## Part 2: Using Hoare logic

Ensure that something like `test.input` works with your system. (It's okay if
you started with hw2 and you have to change the syntax slightly.)

Prove the following programs correct by finding `invariant` annotations for
their loops. 

### Slow multiplication

Turn in an annotated copy of the following program as a file called `slow-mult.input`
with loop invariants such that its assertion passes.

    // compute a * b using repeated addition, and place the result in ans
    assume a >= 0 && b >= 0;
    ans := 0;
    n := a;
    while n > 0
      invariant true // TODO: add invariants so that the assertion below passes
    {
      ans := ans + b;
      n := n - 1
    };
    assert ans = a * b 

### Really slow multiplication

Turn in an annotated copy of the following program as a file called `really-slow-mult.input`
with loop invariants such that its assertion passes.

    // compute a * b using repeated addition (where addition is computed by repeated incrementing), and place the result in ans
    assume a >= 0 && b >= 0;
    ans := 0;
    i := a;
    while i > 0
      invariant true // TODO: add invariants so that the assertion below passes
    {
      j := b;
      while j > 0
        invariant true // TODO: add invariants so that the assertion below passes
      {
        ans := ans + 1;
        j := j - 1
      };
      i := i - 1
    };
    assert ans = a * b 

## Extra credit ideas

There are many ways to add features to this language, or to write more programs.

These ideas are presented in no particular order, and the later ones do not
necessarily depend on the earlier ones.

### Write another integer program

Write and verify a program that computes something interesting over integers and
has a nontrivial specification.

### Add havoc and support "path sensitive" loop invariants

You may have noticed in Part 2 that nested loops do not work so well in our set
up.  Think about why this is. Fix the problem by changing the definition of `wp`
to not use side conditions, but to instead use quantifiers to "havoc" variables
that are modified with the loop.

You will need to add "for all" quantifiers to the syntax of the expression
language. Add them to your Z3 translator. Then you can use them in your new `wp`.

Show off your new feature by demonstrating that you need fewer loop invariants
on the inner loop of "really slow multiplication".

### Add arrays

Add array variables to the language. There are some TODOs sprinkled throughout
the starter code to help you. Check out the documentation for Z3's theory of
arrays, linked from the course website.

### Add quantifiers

Add universal and existential quantifiers to the expression language. Parse them
and support them in your Z3 translator. 

Use quantifiers to write an interesting program over integers whose
specification uses quantifiers.

### Write some interesting programs using arrays and quantifiers

If you add arrays and quantifiers, you have lots of interesting programs to
try. How about some of the ones from Week 4's section that were done in Dafny?
