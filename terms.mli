type location = int

type operator =
  | And
  | Eq
  | Neq
  | Or

type const_term = Unit | Bool of bool | Int of int

type pattern =
  | ConstPat of const_term
  | TuplePat of lpattern list
  | WildPat
and lpattern = location * pattern

type term =
  | Const   of const_term
  | Var     of string
  | App     of lterm * lterm
  | BinOp   of operator * lterm * lterm
  | Tuple   of lterm list
  | IfElse  of { cond : lterm ; ifbr : lterm ; elsebr : lterm }
  | Match   of { expr : lterm ; arms : (lpattern * lterm) list }
  | Abs     of string * lterm
  | LetIn   of string * lterm * lterm
and lterm = location * term

type toplevel_item = LetDef of string option * lterm

val string_of_pattern : lpattern -> string
val string_of_term : lterm -> string

type parse_error =
  | EmptyTerm     of location
  | NoCases       of location
  | SyntaxError   of location * string
  | UnexpectedEOF

val parse : string -> (toplevel_item, parse_error) result
