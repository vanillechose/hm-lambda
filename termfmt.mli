type assoc =
  | Left
  | Right

type 't info =
  | Constant of string
  | Unary of {
    prec : int ;
    prefix : string ;
    suffix : 't ;
  }
  | Binary of {
    prec : int ;
    assoc : assoc ;
    left : 't ;
    infix : string ;
    right : 't ;
  }
  | Nary of {
    prec : int ;
    prefix : string ;
    head : 't ;
    suffix : string ;
    children : (string * 't) list
  }

module type Term = sig
  type t

  val term_info : t -> t info
end

module Make : functor (T : Term) -> sig
  val string_of_t : T.t -> string
end
