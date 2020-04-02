type value = VInt of int | VBool of bool
  [@@deriving show]

type ty = Integer | Boolean
  [@@deriving show]

type expr =
  | Literal of value
  | Bool of value
  | Add of expr * expr
  | And of expr * expr
  | Var of string
  (* the following line automatically generates a function `show_expr : expr -> string` *)
  [@@deriving show]

type binding = Binding of string option * expr
  [@@deriving show]