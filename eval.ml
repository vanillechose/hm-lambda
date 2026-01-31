type code =
  | Load   of value
  | Var    of int
  | Abs    of code
  | App    of code * code
  | Op     of Terms.operator * code * code
  | Branch of { cond : code ; ifbr : code ; elsebr : code }
and value =
  | Unit
  | Bool    of bool
  | Closure of code * env
and env = value list

let string_of_value = function
  | Unit -> "()"
  | Bool b -> "#" ^ if b then "t" else "f"
  | Closure _ -> "<fun>"

let string_of_operator = function
  | Terms.And -> "and"
  | Terms.Eq  -> "eq?"
  | Terms.Neq -> "neq?"
  | Terms.Or  -> "or"

let string_of_code code =
  let ws k = String.make (2 * k) ' ' in
  let rec aux depth = function
    | Load v -> ws depth ^ "load " ^ string_of_value v
    | Var n -> ws depth ^ "var " ^ string_of_int n
    | Abs m -> ws depth ^ "closure\n" ^ aux (depth + 1) m
    | App (m, n) ->
        ws depth ^ "apply\n"
        ^ aux (depth + 1) m
        ^ aux (depth + 1) n
    | Op (f, m, n) ->
        ws depth ^ string_of_operator f ^ "\n"
        ^ aux (depth + 1) m ^ "\n"
        ^ aux (depth + 1) n
    | Branch { cond : code ; ifbr : code ; elsebr : code } ->
        ws depth ^ "if\n"
        ^ aux (depth + 1) cond ^ "\n"
        ^ "then\n"
        ^ aux (depth + 1) ifbr ^ "\n"
        ^ "else\n"
        ^ aux (depth + 1) elsebr
  in
  aux 0 code

let eval ?(trace=false) env code =
  let print_trace depth m =
    if trace then
      print_endline ("(trace) " ^ String.make (4 * depth) ' ' ^ m)
  in
  (* Disable warnings about non-exhaustive pattern matching.
   * At this point we know the expression is well-typed so
   * it should not cause any problem *)
  let[@ocaml.warning "-8"] rec aux depth env code =
    match code with
    | Load v ->
        print_trace depth ("load = " ^ string_of_value v) ;
        v
    | Var n ->
        let v = List.nth env n in
        print_trace depth ("var " ^ string_of_int n ^ " = " ^ string_of_value v) ;
        v
    | Abs m ->
        print_trace depth "closure" ;
        Closure (m, env)
    | App (m, n) ->
        let clo = aux depth env m in
        let arg = aux depth env n in
        (* if the term being evaluated is well-typed, clo is a Closure *)
        let Closure (c, e) = clo in
        print_trace depth "apply {" ;
        let v = aux (depth + 1) (arg :: e) c in
        print_trace depth ("} = " ^ string_of_value v) ;
        v
    | Op ((And | Or as f), m, n) ->
        let Bool u = aux depth env m in
        (* short circuit *)
        if (f = Or) = u then begin
          print_trace depth (string_of_operator f ^ "(short) = " ^ string_of_value (Bool u)) ;
          Bool u
        end else
          let Bool v = aux depth env n in
          let w = if f = And then Bool (u && v) else Bool (u || v) in
          print_trace depth (string_of_operator f ^ " = " ^ string_of_value w) ;
          w
    | Op ((Eq | Neq as f), m, n) ->
        let u = aux depth env m in
        let v = aux depth env n in
        let w =
          begin match u with
            | Closure _ -> Bool false
            | _ -> Bool (if f = Eq then u = v else u <> v)
          end
        in
        print_trace depth (string_of_operator f ^ " = " ^ string_of_value w) ;
        w
    | Branch { cond ; ifbr ; elsebr } ->
        let v = aux depth env cond in
        print_trace depth "branch" ;
        begin match v with
          | Bool true -> aux depth env ifbr
          | Bool false -> aux depth env elsebr
        end
  in
  aux 0 env code
