type 'a value = Val of 'a
  [@@deriving show]

type expr =
  | Literal of (int value)
  | Bool of (bool value)
  | Add of expr * expr
  | And of expr * expr
  (* the following line automatically generates a function `show_expr : expr -> string` *)
  [@@deriving show]
