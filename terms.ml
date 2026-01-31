type location = int

type operator =
  | And
  | Eq
  | Neq
  | Or

type term =
  | Unit
  | Bool    of bool
  | Var     of string
  | App     of lterm * lterm
  | BinOp   of operator * lterm * lterm
  | IfElse  of { cond : lterm ; ifbr : lterm ; elsebr : lterm }
  | Abs     of string * lterm
  | LetIn   of string * lterm * lterm
and lterm = location * term

type toplevel_item =
  | LetDef of string option * lterm

let paren s = "(" ^ s ^ ")"

(*
 * Precedence | Associativity | Operator      |
 *          1 |             X |      *let-in* |
 *         11 |             X |      *lambda* |
 *         16 |             X |     *if-else* |
 *         21 |         right |        &&, || |
 *         31 |         right |         =, <> |
 *         41 |          left | *application* |
 *            |               |               |
 *)
let string_of_term (_, term) =
  (* pmin: minimum precedence required to be printed without parentheses *)
  let rec aux pmin = function
    | Unit -> "()"
    | Bool b -> "#" ^ if b then "t" else "f"
    | Var x -> x
    | App ((_, m), (_, n)) ->
        (* precedence 61, left associative *)
        let s = aux 40 m ^ " " ^ aux 41 n in
        if pmin >= 41 then paren s else s
    | BinOp ((Neq | Eq as f), (_, m), (_, n)) ->
        (* precedence 41, right associative *)
        let s = aux 31 m ^ (if f = Eq then " = " else " <> ") ^ aux 30 n in
        if pmin >= 31 then paren s else s
    | BinOp ((And | Or as f), (_, m), (_, n)) ->
        (* precedence 21, right associative *)
        let s = aux 21 m ^ (if f = And then " && " else " || ") ^ aux 20 n in
        if pmin >= 21 then paren s else s
    | IfElse { cond = (_, m) ; ifbr = (_, n) ; elsebr = (_, o) } ->
        let s = "if " ^ aux 0 m ^ " then " ^ aux 0 n ^ " else " ^ aux 0 o in
        if pmin >= 16 then paren s else s
    | Abs (x, (_, m)) ->
        (* precedence 11 *)
        let s = "\\" ^ x ^ ". " ^ aux 10 m in
        if pmin >= 11 then paren s else s
    | LetIn (x, (_, m), (_, n)) ->
        (* precedence 1 *)
        let s = "let " ^ x ^ " = " ^ aux 0 m ^ " in " ^ aux 0 n in
        if pmin >= 1 then paren s else s
  in
  aux 0 term

type parse_error =
  | EmptyTerm   of location
  | SyntaxError of location * string

(*
 * term: LET IDENT '=' term IN term
 *     | lambda
 *     ;
 *
 * lambda: '\' IDENT '.' term
 *       | if-expr
 *       ;
 *
 * if-expr: IF term THEN term ELSE term
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
 *)
let parse source =
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
          (off, Bool (b = True))
      | Ident x ->
          let  m  = (loc (), Var x) in
          bump () ;
          m
      | LParen ->
          let off = loc () in
          bump () ;
          if tok () = RParen then begin
            bump () ;
            (off, Unit)
          end else
            let m = parse_term () in
            let _ = expect RParen in
            m
      | t ->
          let msg =
            "unexpected " ^ string_of_token t ^ ", expected one of "
            ^ "'#t', '#f', '(', identifier"
          in
          raise (Parse_error (SyntaxError (loc (), msg)))

  and parse_factor stack =
    match tok () with
      | And
      | Else
      | Eof
      | Eq
      | In
      | Neq
      | Or
      | Then
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

  and parse_ifelse () =
    match tok () with
      | If ->
          let off = loc () in
          bump () ;
          let cond   = parse_term () in
          let _      = expect Then in
          let ifbr   = parse_term () in
          let _      = expect Else in
          let elsebr = parse_term () in
          (off, IfElse { cond ; ifbr ; elsebr })
      | _ -> parse_expr ()

  and parse_lambda () =
    match tok () with
      | Lambda ->
          let off  = loc () in
          bump () ;
          let name = expect_ident () in
          let _    = expect Dot in
          let m    = parse_term () in
          (off, Abs (name, m))
      | _ -> parse_ifelse ()

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
      | _ -> parse_lambda ()
  
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

(* {{{ *)
let _old_parser source =
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

  let make_app m (off, _ as n) = (off, App (m, n)) in

  let reduce ?(allow_unit=false) start = function
    | [] -> if allow_unit then (start, Unit) else raise (Parse_error (EmptyTerm start))
    | m :: ms -> List.fold_left make_app m ms
  in

  let rec parse_expr ?(allow_unit=false) ?(in_paren=false) ?(in_let=false) start stack =
    match tok () with
      | RParen when in_paren ->
          reduce ~allow_unit start (List.rev stack)
      | Eof ->
          reduce start (List.rev stack)
      | In when in_let ->
          reduce start (List.rev stack)
      | Lambda ->
          let  off  = loc () in
          bump () ;
          let name = expect_ident () in
          let _    = expect Dot in
          let m    = parse_expr (loc ()) [] ~allow_unit:false ~in_let ~in_paren in
          let abs  = (off, Abs (name, m)) in
          parse_expr start (abs :: stack) ~in_paren ~in_let
      | LParen ->
          let off  = loc () in
          bump () ;
          let m    = parse_expr off [] ~allow_unit:true ~in_paren:true in
          let _    = expect RParen in
          parse_expr start (m :: stack) ~in_paren ~in_let
      | Let ->
          let  off = loc () in
          bump () ;
          let name = expect_ident () in
          let _    = expect Eq in
          let m    = parse_expr (loc ()) [] ~in_let:true in
          let _    = expect In in
          let n    = parse_expr (loc ()) [] ~in_paren ~in_let in
          let leet = (off, LetIn (name, m, n)) in
          parse_expr start (leet :: stack) ~in_paren ~in_let
      | Ident x ->
          let  m    = (loc (), Var x) in
          bump () ;
          parse_expr start (m :: stack) ~in_paren ~in_let
      | True | False ->
          let m = (loc (), Bool (tok () = True)) in
          bump () ;
          parse_expr start (m :: stack) ~in_paren ~in_let
      | t ->
          let msg =
            "unexpected " ^ string_of_token t ^ ", expected one of "
            ^ "'\\', '(', identifier"
            ^ (if in_paren then ", ')'"  else "")
            ^ (if in_let   then ", 'in'" else "")
          in
          raise (Parse_error (SyntaxError (loc (), msg)))
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
    let m = parse_expr (loc ()) [] ~in_let:true in
    match tok (), name with
      | (In, Some name) ->
          bump () ;
          let n = parse_expr (loc ()) [] in
          LetDef (None, (off, LetIn (name, m, n)))
      | (Eof, _) ->
          LetDef (name, m)
      | (t, _) ->
          let msg = "unexpected " ^ string_of_token t ^ ", expected end of file" in
          raise (Parse_error (SyntaxError (loc (), msg)))
  in
  try Ok (parse_item ()) with Parse_error e -> Error e
(* }}} *)
