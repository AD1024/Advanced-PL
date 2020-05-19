let line_col_of_lex_pos =
  let open Lexing in
  function {pos_fname = _; pos_lnum; pos_bol; pos_cnum } ->
    Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol)

let string_of_lex_pos =
  let open Lexing in
  function {pos_fname; pos_lnum = _; pos_bol = _; pos_cnum = _} as pos ->
    Printf.sprintf "%s:%s" pos_fname (line_col_of_lex_pos pos)

let string_of_loc loc  =
  match loc with None -> "" | Some (pos1, pos2) ->
  (* we assume the two positions have the same filename *)
  let open Lexing in
  Printf.sprintf "%s:%s-%s" pos1.pos_fname (line_col_of_lex_pos pos1) (line_col_of_lex_pos pos2)

type location = Lexing.position * Lexing.position

type 'a located =
  { loc: location option [@printer fun fmt loc -> fprintf fmt "%s" (string_of_loc loc)]
  ; value: 'a
  }
    [@@deriving show]

module StringSet = Set.Make(String)

let with_no_loc re = {loc = None; value = re}
let with_loc loc re = {loc; value = re}

module Ty = struct
  type raw_t = Bool | Fun of t * t [@@deriving show]
  and t = raw_t located [@printer fun fmt ty -> pp_raw_t fmt ty.value]

  (* helper functions to construct expressions without locations *)
  let bool = with_no_loc Bool
  let func ty1 ty2 = with_no_loc (Fun (ty1, ty2))

  (* unparsing for types *)
  let pretty ty =
    let rec go needs_parens_around_fun ty =
      match ty.value with
      | Bool -> "bool"
      | Fun (ty1, ty2) ->
         if needs_parens_around_fun
         then Printf.sprintf "(%s)" (go false ty)

         (* because `->` is right associative, parens are not need around any function types to
            the right of an arrow, but they are needed to the left of an arrow. *)
         else Printf.sprintf "%s -> %s" (go true ty1) (go false ty2)
    in go false ty
end

type raw_expr =
  | Var of string
  | Lambda of string * Ty.t * expr
  | App of expr * expr
  | Bool of bool
  | IfThenElse of expr * expr * expr
                                  [@@deriving show]
and expr = raw_expr located [@printer fun fmt e -> pp_raw_expr fmt e.value]

(* helper functions to construct expressions without locations *)
let var x = with_no_loc (Var x)
let lambda x ty e = with_no_loc (Lambda (x, ty, e))
let app e1 e2 = with_no_loc (App (e1, e2))
let bool b = with_no_loc (Bool b)
let ifthenelse e1 e2 e3 = with_no_loc (IfThenElse (e1, e2, e3))

let rec free_vars e =
  match e.value with
  | Var x -> StringSet.singleton x
  | Lambda (x, _, e) -> StringSet.remove x (free_vars e)
  | App (e1, e2) -> StringSet.union (free_vars e1) (free_vars e2)
  | Bool _ -> StringSet.empty
  | IfThenElse (e1, e2, e3) ->
     StringSet.union (free_vars e1)
       (StringSet.union (free_vars e2) (free_vars e3))

(* warning: NOT capture-avoiding, but more like "capture-detecting" :)
 *
 * throws an exception if capture would occur
 *)
let rec subst from to_ e =
  match e.value with
  | Var x -> if x = from then to_ else e
  | Lambda (x, ty, body) ->
     if x = from then e
     else if StringSet.mem x (free_vars to_) then failwith "subst: capture would occur!"
     else lambda x ty (subst from to_ body)
  | App (e1, e2) -> app (subst from to_ e1) (subst from to_ e2)
  | Bool b -> bool b
  | IfThenElse (e1, e2, e3) ->
     ifthenelse (subst from to_ e1) (subst from to_ e2) (subst from to_ e3)

(* return the first index of x in l, or None if x is not in l *)
let rec find_idx_opt (x : 'a)  (l : 'a list) : int option =
  match l with
  | [] -> None
  | x0 :: xs -> if x0 = x then Some 0 else Option.map (fun n -> n + 1) (find_idx_opt x xs)

let fresh avoid varname =
  if not (StringSet.mem varname avoid)
  then varname
  else failwith "fresh is not yet implemented"

(* return booleans indicating whether e1 is alpha equivalent to e2.
 *
 * this is a little tricky to do right, especially if you don't want to use substitution.
 * the approach taken here is essentially to (implicitly and on the fly) convert to de Bruijn indices.
 *
 * it is also possible to use substitution to rename bound variables as you go.
 * the advantage of the on-the-fly-de-Bruijn implementation is that by not relying on substitution,
 * we can use this function to *test* substitution.
 *)
let alpha_equiv e1 e2 =
  (* the helper function `go` takes two additional "contexts" g1 and g2 that describe what names are bound "above us" *)
  let rec go g1 e1 g2 e2 =
    match e1.value, e2.value with
    | Var x1, Var x2 ->
       (* two variables are alpha equivalent if they are both free and have the same name,
          or if they are both bound by the same lambda (same index!) "above us" *)
       begin match find_idx_opt x1 g1, find_idx_opt x2 g2 with
       | None, None -> x1 = x2
       | Some i1, Some i2 -> i1 = i2
       (* otherwise, one is bound and one is free: not equivalent *)
       | _, _ -> false
       end
    | Lambda (x1, ty1, e1), Lambda (x2, ty2, e2) -> ty1 = ty2 && go (x1 :: g1) e1 (x2 :: g2) e2
    | App (e11, e12), App (e21, e22) -> go g1 e11 g2 e21 && go g1 e12 g2 e22
    | Bool b1, Bool b2 -> b1 = b2
    | IfThenElse (e11, e12, e13), IfThenElse (e21, e22, e23) -> go g1 e11 g2 e21 && go g1 e12 g2 e22 && go g1 e13 g2 e23

    (* by listing out the remaining cases explicitly here, we ensure that the
       compiler will warn us when we add new expressions to the language. *)
    | Var _, _ -> false
    | Lambda _, _ -> false
    | App _, _ -> false
    | Bool _, _ -> false
    | IfThenElse _, _ -> false
  in go [] e1 [] e2


type step_result = expr option

(* implements the small-step operational semantics of STLC *)
let rec step (e : expr) : step_result =
  match e.value with
  | Var _ | Lambda _ | Bool _ -> None
  | App (e1, e2) -> begin
      match step e1 with
      | Some e1' -> Some (app e1' e2)
      | None ->
         match step e2 with
         | Some e2' -> Some (app e1 e2')
         | None ->
            match e1.value with
            | Lambda (x, _, e) -> Some (subst x e2 e)
            | _ -> None
    end
  | IfThenElse (e1, e2, e3) ->
     match step e1 with
     | Some e1' -> Some (ifthenelse e1' e2 e3)
     | None ->
        match e1.value with
        | Bool true -> Some e2
        | Bool false -> Some e3
        | _ -> None

(* "normalizes" an STLC expression by performing any available
   computation, even under a lambda.

   WARNING: this implementation assumes substitution to be capture avoiding,
   so it might crash until you actually implement capture avoidance. *)
let rec normalize (e : expr) : expr =
  match e.value with
  | Var _ -> e
  | Lambda (x, ty, e) -> lambda x ty (normalize e)
  | App (e1, e2) -> begin
      let e1' = normalize e1 in
      let e2' = normalize e2 in
      match e1'.value with
      | Lambda (x, _, e) -> normalize (subst x e2' e)
      | _ -> app e1' e2'
    end
  | Bool _ -> e
  | IfThenElse (e1, e2, e3) ->
     let e1' = normalize e1 in
     match e1'.value with
     | Bool true -> normalize e2
     | Bool false -> normalize e3
     | _ -> ifthenelse e1' e2 e3

(* Precedences for unparsing *)
module Prec = struct
  type t = TOP | APP | BOT

  let int_of_t = function
    | TOP -> 0
    | APP -> 1
    | BOT -> 2
  let compare t1 t2 = compare (int_of_t t1) (int_of_t t2)
  type side = LEFT | RIGHT | NONE
  let prec_of_expr e =
    match e.value with
    | Var _ | Bool _ -> BOT
    | Lambda _ -> TOP
    | App _ -> APP
    | IfThenElse _ -> TOP
  let assoc_of_expr e =
    match e.value with
    | Var _ | Bool _ -> NONE
    | Lambda _ | IfThenElse _ -> NONE
    | App _ -> LEFT

  let needs_parens prec side e =
    not (compare prec (prec_of_expr e) < 0 ||
           (compare prec (prec_of_expr e) = 0 &&
              side = assoc_of_expr e))
end
(* unparsing *)
let pretty e =
  let rec go prec side e =
    if Prec.needs_parens prec side e then Printf.sprintf "(%s)" (go Prec.TOP Prec.NONE e)
    else
      match e.value with
      | Var x -> x
      | Lambda (x, ty, e) ->
         Printf.sprintf "\\%s:%s. %s" x (Ty.pretty ty) (go Prec.TOP Prec.NONE e)
      | App (e1, e2) ->
         Printf.sprintf "%s %s" (go Prec.APP Prec.LEFT e1) (go Prec.APP Prec.RIGHT e2)
      | Bool b -> string_of_bool b
      | IfThenElse (e1, e2, e3) ->
         Printf.sprintf "if %s then %s else %s"
           (go Prec.TOP Prec.NONE e1)
           (go Prec.TOP Prec.NONE e2)
           (go Prec.TOP Prec.NONE e3)
  in go Prec.TOP Prec.NONE e

type binding =
  | Val of string * expr
  | Eval of expr
