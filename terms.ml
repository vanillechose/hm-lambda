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
and lpattern =location * pattern

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

type toplevel_item =
  | LetDef of string option * lterm

let string_of_pattern (_, p) =
  let module Info =
    struct
      type t = pattern

      open Termfmt

      let term_info = function
        | ConstPat Int k -> Constant (string_of_int k)
        | ConstPat Unit -> Constant "()"
        | ConstPat Bool x -> Constant ("#" ^ if x then "t" else "f")
        | WildPat -> Constant "_"
        | TuplePat [] -> Constant "<empty tuple>"
        | TuplePat ((_, p) :: ps)->
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
        | Const Int k -> Constant (string_of_int k)
        | Const Unit -> Constant "()"
        | Const Bool x -> Constant ("#" ^ if x then "t" else "f")
        | Var x -> Constant x

        | App ((_, left), (_, right)) ->
            Binary { prec = 41 ; assoc = Left ; left ;
              infix = " " ; right }
        | BinOp ((Neq | Eq as f), (_, left), (_, right)) ->
            Binary { prec = 41 ; assoc = Right ; left ;
              infix = if f = Eq then " = " else " <> " ; right }
        | BinOp ((And | Or as f), (_, left), (_, right)) ->
            Binary { prec = 21 ; assoc = Right ; left ;
              infix = if f = And then " && " else " || " ; right }
        | Tuple [] -> Constant "<empty tuple>"
        | Tuple ((_, m) :: ns) ->
            Nary { prec = 19 ; prefix = "" ; head = m ;
              suffix = "" ; children = List.map (fun (_, n) -> (", ", n)) ns }

        | IfElse { cond = (_, m) ; ifbr = (_, n) ; elsebr = (_, o) } ->
            Nary { prec = 1 ; prefix = "if " ; head = m ;
              suffix = "" ; children = [ " then ", n ; " else ", o ] }
        | Abs (x, (_, m)) ->
            Unary { prec = 1 ; prefix = "\\" ^ x ^ ". " ; suffix = m }
        | LetIn (x, (_, m), (_, n)) ->
            Nary { prec = 1 ; prefix = "let " ^ x ^ " = " ; head = m ;
              suffix = "" ; children = [ " in ", n ] }
        | Match { expr = (_, m) ; arms } ->
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
  | EmptyTerm   of location
  | NoCases     of location
  | SyntaxError of location * string

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
    end else
      let msg = "expected " ^ string_of_token t ^ ", found " ^ string_of_token t' in
      raise (Parse_error (SyntaxError (loc (), msg)))
  in

  let expect_ident () =
    match tok () with
      | Ident x -> bump () ; x
      | t ->
          let msg = "expected identifier, found " ^ string_of_token t in
          raise (Parse_error (SyntaxError (loc (), msg)))
  in
  (* }}} *)

  let make_app (off, _ as m) n = (off, App (m, n)) in

  let reduce start = function
    | [] -> raise (Parse_error (EmptyTerm start))
    | m :: ms -> List.fold_left make_app m ms
  in

  let rec parse_atom () =
    match tok () with
      | True | False as b ->
          let off = loc () in
          bump () ;
          (off, Const (Bool (b = True)))
      | Ident x ->
          let  m  = (loc (), Var x) in
          bump () ;
          m
      | Integer k ->
          let m = (loc (), Const (Int k)) in
          bump () ;
          m
      | LParen ->
          let off = loc () in
          bump () ;
          if tok () = RParen then begin
            bump () ;
            (off, Const Unit)
          end else
            let m = parse_term () in
            let _ = expect RParen in
            m
      | t ->
          let msg =
            "unexpected " ^ string_of_token t ^ "in expression, expected one of "
            ^ "'#t', '#f', '(', identifier"
          in
          raise (Parse_error (SyntaxError (loc (), msg)))

  and parse_factor stack =
    match tok () with
      | And
      | Comma
      | Else
      | Eof
      | Eq
      | In
      | Neq
      | Or
      | Then
      | With
      | Case
      | RParen ->
          reduce (loc ()) (List.rev stack)
      | _ -> let m = parse_atom () in parse_factor (m :: stack)

  and parse_test () =
    let m = parse_factor [] in
    match tok () with
      | Eq | Neq as f ->
          let off = loc () in
          bump () ;
          let n   = parse_test () in
          let op  = (if f = Eq then Eq else Neq : operator) in
          (off, BinOp (op, m, n))
      | _ -> m

  and parse_expr () =
    let m = parse_test () in
    match tok () with
      | And | Or as f ->
          let off = loc () in
          bump () ;
          let n   = parse_expr () in
          let op  = (if f = And then And else Or : operator) in
          (off, BinOp (op, m, n))
      | _ -> m

  and parse_tuple () =
    let m = parse_expr () in
    match tok () with
      | Comma ->
          bump () ;
          begin match tok (), parse_tuple () with
            | t, (_, Tuple ms) when t <> LParen -> (fst m, Tuple (m :: ms))
            | _, n -> (fst m, Tuple [ m ; n ])
          end
      | _ -> m

  and parse_pattern () =
    match tok () with
      | True | False as b ->
          let off = loc () in
          bump () ;
          (off, ConstPat (if b = True then Bool true else Bool false))
      | Integer k ->
          let off = loc () in
          bump () ;
          (off, ConstPat (Int k))
      | Wildcard ->
          let off = loc () in
          bump () ;
          (off, WildPat)
      | LParen ->
          let off = loc () in
          bump () ;
          if tok () = RParen then begin
            bump () ;
            (off, ConstPat Unit)
          end else
            let p = parse_tuple_pattern () in
            let _ = expect RParen in
            p
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
            | t, (_, TuplePat ps) when t <> LParen -> (fst p, TuplePat (p :: ps))
            | _, q -> (fst p, TuplePat [ p ; q ])
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
          (off, LetIn (name, m, n))
      | Lambda ->
          let off = loc () in
          bump () ;
          let name = expect_ident () in
          let _    = expect Dot in
          let m    = parse_term () in
          (off, Abs (name, m))
      | If ->
          let off = loc () in
          bump () ;
          let cond   = parse_term () in
          let _      = expect Then in
          let ifbr   = parse_term () in
          let _      = expect Else in
          let elsebr = parse_term () in
          (off, IfElse { cond ; ifbr ; elsebr })
      | Match ->
          let off  = loc () in
          bump () ;
          let m    = parse_term () in
          let _    = expect With in
          let arms = match_arms [] in
          if arms = [] then
            raise (Parse_error (NoCases off))
          else
            (off, Match { expr = m ; arms })
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
            LetDef (None, (off, LetIn (name, m, n)))
        | (Eof, _) ->
            LetDef (name, m)
        | (t, _) ->
            let msg = "unexpected " ^ string_of_token t ^ ", expected end of file"
              ^ if name <> None then " or 'in'" else "" in
            raise (Parse_error (SyntaxError (loc (), msg)))
    in
    expect Eof ;
    def
  in
  try Ok (parse_item ()) with Parse_error e -> Error e
