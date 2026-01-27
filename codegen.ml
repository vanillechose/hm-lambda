open Eval

module SMap = Map.Make(String)

(* - count is the number of bindings in the environment ;
 * - binding maps a name to its index starting from the end of the environment. *)
type layout = { count : int ; bindings : int SMap.t }

let empty_layout = { count = 0 ; bindings = SMap.empty }

let letin_to_app (term : Terms.lterm) =
  match term with
    | (_, LetIn (x, m, n)) -> (0, Terms.App ((0, Terms.Abs (x, n)), m))
    | _ -> term

let bind name layout =
  let n = layout.count in
  { count = n + 1 ; bindings = SMap.add name (n + 1) layout.bindings }

let idx name layout =
  let n = SMap.find name layout.bindings in
  layout.count - n

let codegen layout (item : Terms.toplevel_item) =
  let rec aux layout (term : Terms.lterm) =
    match term with
      | (_, Unit) ->
          Load Unit
      | (_, Bool b) ->
          Load (Bool b)
      | (_, Var x) ->
          Var (idx x layout)
      | (_, BinOp (f, m, n)) ->
          Op (f, aux layout m, aux layout n)
      | (_, Abs (x, m)) ->
          let m = aux (bind x layout) m in
          Abs m
      | (_, App (m, n)) ->
          App (aux layout m, aux layout n)
      | (_, LetIn _) ->
          let m = letin_to_app term in
          aux layout m
  in
  match item with
    | LetDef (Some name, term) ->
        let code = aux layout term in
        (code, bind name layout)
    | LetDef (None, term) ->
        let code = aux layout term in
        (code, layout)
