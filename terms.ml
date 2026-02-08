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

type toplevel_item =
  | Pletdef of string option * lterm

let string_of_pattern (_, p) =
  let module Info =
    struct
      type t = pattern

      open Termfmt

      let term_info = function
        | Pconstpat Aint k -> Constant (string_of_int k)
        | Pconstpat Aunit -> Constant "()"
        | Pconstpat Abool x -> Constant ("#" ^ if x then "t" else "f")
        | Pvarpat x -> Constant x
        | Pwildpat -> Constant "_"
        | Ptuplepat [] -> Constant "<empty tuple>"
        | Ptuplepat ((_, p) :: ps)->
            Nary { prec = 2 ; prefix = "" ; head = p ;
              suffix = "" ; children = List.map (fun (_, n) -> (", ", n)) ps }          
    end
  in
  let module F = Termfmt.Make(Info) in
  F.string_of_t p

let string_of_term (_, m) =
  let module Info =
    struct
      type t = term

      open Termfmt

      let term_info = function
        | Pconst Aint k -> Constant (string_of_int k)
        | Pconst Aunit -> Constant "()"
        | Pconst Abool x -> Constant ("#" ^ if x then "t" else "f")
        | Pvar x -> Constant x

        | Papp ((_, left), (_, right)) ->
            Binary { prec = 41 ; assoc = Left ; left ;
              infix = " " ; right }
        | Pprim ((Oneq | Oeq as f), (_, left), (_, right)) ->
            Binary { prec = 41 ; assoc = Right ; left ;
              infix = if f = Oeq then " = " else " <> " ; right }
        | Pprim ((Oand | Oor as f), (_, left), (_, right)) ->
            Binary { prec = 21 ; assoc = Right ; left ;
              infix = if f = Oand then " && " else " || " ; right }
        | Ptuple [] -> Constant "<empty tuple>"
        | Ptuple ((_, m) :: ns) ->
            Nary { prec = 19 ; prefix = "" ; head = m ;
              suffix = "" ; children = List.map (fun (_, n) -> (", ", n)) ns }

        | Pifelse { cond = (_, m) ; ifbr = (_, n) ; elsebr = (_, o) } ->
            Nary { prec = 1 ; prefix = "if " ; head = m ;
              suffix = "" ; children = [ " then ", n ; " else ", o ] }
        | Plambda (x, (_, m)) ->
            Unary { prec = 1 ; prefix = "\\" ^ x ^ ". " ; suffix = m }
        | Pletin (x, (_, m), (_, n)) ->
            Nary { prec = 1 ; prefix = "let " ^ x ^ " = " ; head = m ;
              suffix = "" ; children = [ " in ", n ] }
        | Pmatch { expr = (_, m) ; arms } ->
            let children =
              List.map (fun (p, (_, n)) ->
                ("\ncase " ^ string_of_pattern p ^ " -> ", n)
              ) arms
            in
            Nary { prec = 1 ; prefix = "match " ; head = m ;
              suffix = " with " ; children }
    end
  in
  let module F = Termfmt.Make(Info) in
  F.string_of_t m

type parse_error =
  | EmptyTerm     of location
  | NoCases       of location
  | SyntaxError   of location * string
  | UnexpectedEOF

(*
 * {{{ lang
 * term: LET IDENT '=' term IN term
 *     | '\' IDENT '.' term
 *     | IF term THEN term ELSE term
 *     | MATCH term WITH { CASE pattern '->' term }
 *     | expr
 *     ;
 *
 * pattern: constant-pattern { ',' constant-pattern }
 *        ;
 *
 * constant-pattern: '#t' | '#f' | '(' ')'
 *                 ;
 *
 * tuple: expr { ',' expr }
 *      ;
 *
 * expr: test { '&&' test }
 *     | test { '||' test }
 *     ;
 *
 * test: factor { '=' factor }
 *     | factor { '<>' factor }
 *     ;
 *
 * factor: factor atom
 *       | atom
 *       ;
 *
 * atom: '(' term ')'
 *       | IDENT
 *       | '#t' | '#f' | '(' ')'
 *       ;
 * }}}
 *)
let parse source =
  (* {{{ lexbuf *)
  let open Lexer in

  let exception Parse_error of parse_error in

  let tok, loc, bump =
    let l = Lexing.from_string source in
    (* current token, current offset *)
    let peek =
      ref (Lexer.token l, Lexing.lexeme_start l)
    in
    let tok () = fst !peek in
    let loc () = snd !peek in
    let bump () =
      let tok = Lexer.token l in
      let loc = Lexing.lexeme_start l in
      peek := (tok, loc) ;
    in
    (tok, loc, bump)
  in
  
  let expect t =
    let t' = tok () in
    if t = t' then begin
      bump () ;
      ()
    end else if t' = Eof then
      raise (Parse_error UnexpectedEOF)
    else
      let msg = "expected " ^ string_of_token t ^ ", found " ^ string_of_token t' in
      raise (Parse_error (SyntaxError (loc (), msg)))
  in

  let expect_ident () =
    match tok () with
      | Ident x -> bump () ; x
      | Eof -> raise (Parse_error UnexpectedEOF)
      | t ->
          let msg = "expected identifier, found " ^ string_of_token t in
          raise (Parse_error (SyntaxError (loc (), msg)))
  in
  (* }}} *)

  let make_app (off, _ as m) n = (off, Papp (m, n)) in

  let reduce start = function
    | [] -> raise (Parse_error (EmptyTerm start))
    | m :: ms -> List.fold_left make_app m ms
  in

  let rec parse_atom () =
    match tok () with
      | True | False as b ->
          let off = loc () in
          bump () ;
          (off, Pconst (Abool (b = True)))
      | Ident x ->
          let  m  = (loc (), Pvar x) in
          bump () ;
          m
      | Integer k ->
          let m = (loc (), Pconst (Aint k)) in
          bump () ;
          m
      | LParen ->
          let off = loc () in
          bump () ;
          if tok () = RParen then begin
            bump () ;
            (off, Pconst Aunit)
          end else
            let m = parse_term () in
            let _ = expect RParen in
            m
      | Eof ->
          raise (Parse_error UnexpectedEOF)
      | t ->
          let msg =
            "unexpected " ^ string_of_token t ^ " in expression, expected one of "
            ^ "'#t', '#f', '(', identifier"
          in
          raise (Parse_error (SyntaxError (loc (), msg)))

  and parse_factor stack =
    match tok () with
      | And
      | Comma
      | Else
      | Eq
      | In
      | Neq
      | Or
      | Then
      | With
      | Case
      | RParen
      | Semisemi ->
          reduce (loc ()) (List.rev stack)
      | _ -> let m = parse_atom () in parse_factor (m :: stack)

  and parse_test () =
    let m = parse_factor [] in
    match tok () with
      | Eq | Neq as f ->
          let off = loc () in
          bump () ;
          let n   = parse_test () in
          let op  = (if f = Eq then Oeq else Oneq : atomic_op) in
          (off, Pprim (op, m, n))
      | _ -> m

  and parse_expr () =
    let m = parse_test () in
    match tok () with
      | And | Or as f ->
          let off = loc () in
          bump () ;
          let n   = parse_expr () in
          let op  = (if f = And then Oand else Oor : atomic_op) in
          (off, Pprim (op, m, n))
      | _ -> m

  and parse_tuple () =
    let m = parse_expr () in
    match tok () with
      | Comma ->
          bump () ;
          begin match tok (), parse_tuple () with
            | t, (_, Ptuple ms) when t <> LParen -> (fst m, Ptuple (m :: ms))
            | _, n -> (fst m, Ptuple [ m ; n ])
          end
      | _ -> m

  and parse_pattern () =
    match tok () with
      | True | False as b ->
          let off = loc () in
          bump () ;
          (off, Pconstpat (if b = True then Abool true else Abool false))
      | Integer k ->
          let off = loc () in
          bump () ;
          (off, Pconstpat (Aint k))
      | Ident x ->
          let off = loc () in
          bump () ;
          (off, Pvarpat x)
      | Wildcard ->
          let off = loc () in
          bump () ;
          (off, Pwildpat)
      | LParen ->
          let off = loc () in
          bump () ;
          if tok () = RParen then begin
            bump () ;
            (off, Pconstpat Aunit)
          end else
            let p = parse_tuple_pattern () in
            let _ = expect RParen in
            p
      | Eof ->
          raise (Parse_error UnexpectedEOF)
      | t ->
          let msg =
            "unexpected " ^ string_of_token t ^ " in pattern, expected one of "
            ^ "'#t', '#f', '('"
          in
          raise (Parse_error (SyntaxError (loc (), msg)))

  and parse_tuple_pattern () =
    let p = parse_pattern () in
    match tok () with
      | Comma ->
          bump () ;
          begin match tok (), parse_tuple_pattern () with
            | t, (_, Ptuplepat ps) when t <> LParen -> (fst p, Ptuplepat (p :: ps))
            | _, q -> (fst p, Ptuplepat [ p ; q ])
          end
      | _ -> p

  and match_arms acc =
    match tok () with
      | Case ->
          bump () ;
          let p = parse_tuple_pattern () in
          let _ = expect Arrow in
          let m = parse_term () in
          match_arms ((p, m) :: acc)
      | Eof -> raise (Parse_error UnexpectedEOF)
      | _ -> List.rev acc

  and parse_term () =
    match tok () with
      | Let ->
          let off  = loc () in
          bump () ;
          let name = expect_ident () in
          let _    = expect Eq in
          let m    = parse_term () in
          let _    = expect In in
          let n    = parse_term () in
          (off, Pletin (name, m, n))
      | Lambda ->
          let off = loc () in
          bump () ;
          let name = expect_ident () in
          let _    = expect Dot in
          let m    = parse_term () in
          (off, Plambda (name, m))
      | If ->
          let off = loc () in
          bump () ;
          let cond   = parse_term () in
          let _      = expect Then in
          let ifbr   = parse_term () in
          let _      = expect Else in
          let elsebr = parse_term () in
          (off, Pifelse { cond ; ifbr ; elsebr })
      | Match ->
          let off  = loc () in
          bump () ;
          let m    = parse_term () in
          let _    = expect With in
          let arms = match_arms [] in
          if arms = [] then
            raise (Parse_error (NoCases off))
          else
            (off, Pmatch { expr = m ; arms })
      | _ -> parse_tuple ()
  
  and parse_item () =
    let off = loc () in
    let name =
      if tok () = Let then begin
        bump () ;
        let name = expect_ident () in
        let _    = expect Eq in
        Some name
      end else
        None
    in
    let m = parse_term () in
    let def =
      match tok (), name with
        | (In, Some name) ->
            bump () ;
            let n = parse_term () in
            Pletdef (None, (off, Pletin (name, m, n)))
        | (Semisemi, _) ->
            Pletdef (name, m)
        | (Eof, _) ->
            raise (Parse_error UnexpectedEOF)
        | (t, _) ->
            let msg = "unexpected " ^ string_of_token t ^ ", expected end of file"
              ^ if name <> None then " or 'in'" else "" in
            raise (Parse_error (SyntaxError (loc (), msg)))
    in
    expect Semisemi ;
    def
  in
  try Ok (parse_item ()) with Parse_error e -> Error e
