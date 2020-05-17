type value =
  | VInt of int
  | VBool of bool

  (* TODO: if you choose to do the array extra credit, uncomment this line *)
  (* | VArray of value list *)

  [@@deriving show]

type unop = Not | Neg [@@deriving show]

type binop =
  | Add
  | Sub
  | Mul
  | Div

  | And
  | Or
  | Implies

  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  [@@deriving show]

let line_col_of_lex_pos =
  let open Lexing in
  function {pos_fname = _; pos_lnum; pos_bol; pos_cnum } ->
    Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol)

let string_of_lex_pos =
  let open Lexing in
  function {pos_fname; pos_lnum = _; pos_bol = _; pos_cnum = _} as pos ->
    Printf.sprintf "%s:%s" pos_fname (line_col_of_lex_pos pos)

let string_of_loc loc =
  match loc with
  | None -> ""
  | Some (pos1, pos2) -> 
     (* we assume the two positions have the same filename *)
     let open Lexing in
     Printf.sprintf "%s:%s-%s" pos1.pos_fname (line_col_of_lex_pos pos1) (line_col_of_lex_pos pos2)

type location = Lexing.position * Lexing.position

type 'a located =
  { loc: location option [@printer fun fmt loc -> fprintf fmt "%s" (string_of_loc loc)]
  ; value: 'a
  }
  [@@deriving show]

type raw_expr =
  | Literal of value
  | Var of string

  | Unop of unop * expr
  | Binop of expr * binop * expr
  [@@deriving show]
and expr = raw_expr located [@printer fun fmt e -> pp_raw_expr fmt e.value]

type ty =
  | TInt
  | TBool
  | TArray of ty
  [@@deriving show]

type raw_stmt =
  | Skip
  | Assign of string * expr
  | Assert of expr
  | Assume of expr
  | Seq of stmt * stmt
  | If of expr * stmt * stmt
  | While of expr * expr list * stmt
  [@@deriving show]
and stmt = raw_stmt located [@printer fun fmt s -> pp_raw_stmt fmt s.value]
