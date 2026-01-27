type location = int

type operator =
  | And
  | Eq
  | Neq
  | Or

type term =
  | Unit
  | Bool  of bool
  | Var   of string
  | App   of lterm * lterm
  | BinOp of operator * lterm * lterm
  | Abs   of string * lterm
  | LetIn of string * lterm * lterm
and lterm = location * term

type toplevel_item = LetDef of string option * lterm

val string_of_term : lterm -> string

type parse_error =
  | EmptyTerm   of location
  | SyntaxError of location * string

val parse : string -> (toplevel_item, parse_error) result
