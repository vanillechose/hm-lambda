(* tree-walk interpreter code: λ-terms represented using de Bruijn indices *)
type code =
  | Load      of value
  | Var       of int
  | Abs       of code
  | App       of code * code
  (* Arithmetic operations *)
  | Op        of Terms.operator * code * code
  | Branch    of { cond : code ; ifbr : code ; elsebr : code }
  | Fail      of string
  | MakeBlock of code list
  | Access    of int * code
and value = 
  | Unit
  | Bool    of bool
  | Int     of int
  | Block   of value list
  | Closure of code * env
and env = value list

val string_of_value : value -> string
val string_of_code : code -> string

val eval : ?trace:bool -> env -> code -> (value, unit) result
