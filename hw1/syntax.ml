type value = VInt of int | VBool of bool
  [@@deriving show]

type expr =
  | Literal of value
  | Bool of value
  | Add of expr * expr
  | And of expr * expr
  (* the following line automatically generates a function `show_expr : expr -> string` *)
  [@@deriving show]
