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

module Make(T : Term) = struct
  open T

  let paren s = "(" ^ s ^ ")"

  let string_of_t term =
    (* pmin: minimum precedence required to be printed without parentheses *)
    let rec aux pmin term =
      match term_info term with
        | Constant s -> s
        | Unary { prec ; prefix ; suffix } ->
            let s = prefix ^ aux (prec - 1) suffix in
            if pmin >= prec then paren s else s
        | Binary { prec ; assoc = Left ; left ; infix ; right } ->
            let s = aux (prec - 1) left ^ infix ^ aux prec right in
            if pmin >= prec then paren s else s
        | Binary { prec ; assoc = Right ; left ; infix ; right } ->
            let s = aux prec left ^ infix ^ aux (prec - 1) right in
            if pmin >= prec then paren s else s
        | Nary { prec ; prefix ; head ; suffix ; children } ->
            let children = List.map (fun (p, t) -> p ^ aux prec t) children in
            let head = prefix ^ aux (prec - 1) head ^ suffix in
            let s = List.fold_left (^) head children in
            if pmin >= prec then paren s else s
    in
    aux 0 term
end
