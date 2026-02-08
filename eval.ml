type code =
  | Quote     of value
  | Var       of int
  | Abs       of code
  | App       of code * code
  | Op        of Terms.atomic_op * code * code
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

let rec string_of_value = function
  | Unit -> "()"
  | Bool b -> "#" ^ if b then "t" else "f"
  | Int k -> string_of_int k
  | Closure _ -> "<fun>"
  | Block [] -> "<empty block>"
  | Block (v :: []) -> "<block containing " ^ string_of_value v ^ ">"
  | Block (v :: vs) ->
      "(" ^ List.fold_left (fun x y ->
        x ^ ", " ^ string_of_value y
      ) (string_of_value v) vs ^ ")"

let string_of_operator : Terms.atomic_op -> string = function
  | Oand -> "and"
  | Oeq  -> "eq?"
  | Oneq -> "neq?"
  | Oor  -> "or"

let string_of_code code =
  let ws k = String.make (2 * k) ' ' in
  let rec aux depth = function
    | Quote v -> ws depth ^ "load " ^ string_of_value v
    | Var n -> ws depth ^ "var " ^ string_of_int n
    | Abs m -> ws depth ^ "closure\n" ^ aux (depth + 1) m
    | App (m, n) ->
        ws depth ^ "apply\n"
        ^ aux (depth + 1) m ^ "\n"
        ^ aux (depth + 1) n
    | Op (f, m, n) ->
        ws depth ^ string_of_operator f ^ "\n"
        ^ aux (depth + 1) m ^ "\n"
        ^ aux (depth + 1) n
    | Branch { cond : code ; ifbr : code ; elsebr : code } ->
        ws depth ^ "if\n"
        ^ aux (depth + 1) cond ^ "\n"
        ^ ws depth ^ "then\n"
        ^ aux (depth + 1) ifbr ^ "\n"
        ^ ws depth ^ "else\n"
        ^ aux (depth + 1) elsebr
    | Fail msg ->
        ws depth ^ "fail " ^ msg
    | MakeBlock ms ->
        let s = List.map (aux (depth + 1)) ms in
        ws depth ^ "makeblock\n"
        ^ List.fold_left (fun x y -> x ^ "\n" ^ y) (List.hd s) (List.tl s)
    | Access (k, m) ->
        ws depth ^ "access " ^ string_of_int k ^ "\n"
        ^ aux (depth + 1) m
  in
  aux 0 code

let eval ?(trace=false) env code =
  let print_trace depth m =
    if trace then
      print_endline ("(trace) " ^ String.make (4 * depth) ' ' ^ m)
  in
  let exception Failure in
  (* Disable warnings about non-exhaustive pattern matching.
   * At this point we know the expression is well-typed so
   * it should not cause any problem *)
  let[@ocaml.warning "-8"] rec aux depth env code =
    match code with
    | Quote v ->
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
    | Op ((Oand | Oor as f), m, n) ->
        let Bool u = aux depth env m in
        (* short circuit *)
        if (f = Oor) = u then begin
          print_trace depth (string_of_operator f ^ "(short) = " ^ string_of_value (Bool u)) ;
          Bool u
        end else
          let Bool v = aux depth env n in
          let w = if f = Oand then Bool (u && v) else Bool (u || v) in
          print_trace depth (string_of_operator f ^ " = " ^ string_of_value w) ;
          w
    | Op ((Oeq | Oneq as f), m, n) ->
        let u = aux depth env m in
        let v = aux depth env n in
        let w =
          begin match u with
            | Closure _ -> Bool false
            | _ -> Bool (if f = Oeq then u = v else u <> v)
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
    | Fail msg ->
        print_endline ("failure: " ^ msg) ;
        raise Failure
    | MakeBlock ms ->
        let v = Block (List.map (aux depth env) ms) in
        print_trace depth ("makeblock = " ^ string_of_value v) ;
        v
    | Access (k, m) ->
        let Block vs = aux depth env m in
        let v = List.nth vs k in
        print_trace depth ("access " ^ string_of_int k ^ " = " ^ string_of_value v) ;
        v
  in
  try Ok (aux 0 env code) with Failure -> Error ()
