open Hw5lib
open Syntax

(* list of test failure messages to print later. *)
let errors: string list ref = ref []

(* given an "equivalence checker", two answers, a test name, and a
   to_string function, check if the two answers are equivalent, and if
   not, record an error message to print later that the test name and the string representations of the two answers. *)
let check_equiv
      (equiv : 'a -> 'a -> bool)
      (expected : 'a)
      (actual : 'a)
      ~(test_name:string)  (* the tilde indicates a "named argument", which you
                              might want to look up if you're not familiar. *)
      (pp : 'a -> string) : unit =
  if not (equiv expected actual)
  then errors := Printf.sprintf "%s: expected something equivalent to '%s' but got actual '%s'" test_name (pp expected) (pp actual) :: !errors

let () =
  let e = lambda "x" Ty.bool (var "y") in
  print_endline (pretty e);
  (* (\x.y)[z/y] should be alpha equivalent to (\w. z) *)
  (* this one should be easy, no capture will occur *)
  let e' = subst "y" (var "z") e in
  print_endline (pretty e');
  check_equiv alpha_equiv (lambda "w" Ty.bool (var "z")) e' ~test_name:"subst 0" pretty


let () =
  let e = lambda "x" Ty.bool (var "y") in
  print_endline (pretty e);
  (* (\x.y)[x/y] should be alpha equivalent to (\z. x) *)
  (* if subst is not careful, x will be "captured" by the binder \x. *)
  let e' = subst "y" (var "x") e in
  print_endline (pretty e');
  check_equiv alpha_equiv (lambda "z" Ty.bool (var "x")) e' ~test_name:"subst 1" pretty

let () =
  let x0 = fresh (StringSet.singleton "x") "x" in 
  let e = lambda "x" Ty.bool (var x0) in
  print_endline (pretty e);
  (* (\x.x0)[x/y] should do nothing (ie, be alpha equivalent to (\x.x0) itself) *)
  (* but if subst tries to rename x -> x0 to avoid capture in the thing being plugged in,
     it will inadvertently capture x0 in the body of the lambda. *)
  let e' = subst "y" (var "x") e in
  print_endline (pretty e');
  check_equiv alpha_equiv (lambda "z" Ty.bool (var "x0")) e' ~test_name:"subst 2" pretty

let () =
  let x0 = fresh (StringSet.singleton "x") "x" in 
  let e = lambda "x" Ty.bool (var "x") in
  print_endline (pretty e);
  (* (\x.x)[x/x0] should do nothing (ie, be alpha equivalent to (\x.x) itself) *)
  (* but if subst tries to rename x -> x0 to avoid capture in the thing being plugged in,
     it will inadvertently introduce a bunch of instances of x0 that will be incorrectly
     substituted out by the recursive call *)
  let e' = subst x0 (var "x") e in
  print_endline (pretty e');
  check_equiv alpha_equiv (lambda "z" Ty.bool (var "z")) e' ~test_name:"subst 3" pretty



(* after all the tests have run, print all the failures. *)
let () =
  match !errors with
  | [] -> ()
  | _ -> List.iter print_endline (List.rev !errors); exit 1
