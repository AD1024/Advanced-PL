let line_col_of_lex_pos =
  let open Lexing in
  function {pos_fname = _; pos_lnum; pos_bol; pos_cnum } ->
    Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol)

let string_of_lex_pos =
  let open Lexing in
  function {pos_fname; pos_lnum = _; pos_bol = _; pos_cnum = _} as pos ->
    Printf.sprintf "%s:%s" pos_fname (line_col_of_lex_pos pos)

let string_of_loc (pos1, pos2) =
  (* we assume the two positions have the same filename *)
  let open Lexing in
  Printf.sprintf "%s:%s-%s" pos1.pos_fname (line_col_of_lex_pos pos1) (line_col_of_lex_pos pos2)

type location = Lexing.position * Lexing.position

type 'a located =
  { loc: location option [@printer fun fmt loc -> match loc with None -> () | Some loc -> fprintf fmt "%s" (string_of_loc loc)]
  ; value: 'a
  }
  [@@deriving show]

(* Useful for manipulating sets of strings (like the set of free
   variables). Check out the OCaml stdlib documentation for Set. *)
module StringSet = Set.Make(String)

type raw_expr =
  | Lam of string * expr
  | App of expr * expr
  | Var of string
  [@@deriving show]
and expr = raw_expr located [@printer fun fmt e -> pp_raw_expr fmt e.value]

type binding =
  | Binding of string option * expr
  [@@deriving show]

(* Useful if you want to build an expr, and all you've got is a
   raw_expr, and it didn't come directly from the parser... *)
let with_no_loc re = {loc = None; value = re}
