type location = int

type atomic_op =
  | Oand
  | Oeq
  | Oneq
  | Oor

type atomic_val =
  | Aunit
  | Abool of bool
  | Aint  of int

type pattern =
  | Pconstpat of atomic_val
  | Ptuplepat of lpattern list
  | Pvarpat   of string
  | Pwildpat
and lpattern = location * pattern

type term =
  | Pconst  of atomic_val
  | Pvar    of string
  | Papp    of lterm * lterm
  | Pprim   of atomic_op * lterm * lterm
  | Ptuple  of lterm list
  | Pifelse of { cond : lterm ; ifbr : lterm ; elsebr : lterm }
  | Pmatch  of { expr : lterm ; arms : (lpattern * lterm) list }
  | Plambda of string * lterm
  | Pletin  of string * lterm * lterm
and lterm = location * term

type toplevel_item = Pletdef of string option * lterm

val string_of_pattern : lpattern -> string
val string_of_term : lterm -> string

type parse_error =
  | EmptyTerm     of location
  | NoCases       of location
  | SyntaxError   of location * string
  | UnexpectedEOF

val parse : string -> (toplevel_item, parse_error) result
