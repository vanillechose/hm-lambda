open Eval

module SMap = Map.Make(String)

(* - count is the number of bindings in the environment ;
 * - binding maps a name to its index starting from the end of the environment.
 * - Reserved L is used to reserve a slot in L without binding a name *)
type layout =
  | Simple   of { count : int ; bindings : int SMap.t }
  | Reserved of layout

let empty_layout = Simple { count = 0 ; bindings = SMap.empty }

let letin_to_app (term : Terms.lterm) =
  match term with
    | (_, LetIn (x, m, n)) -> (0, Terms.App ((0, Terms.Abs (x, n)), m))
    | _ -> term

let bind name layout =
  let rec aux dis = function
    | Simple layout ->
        let n = layout.count in
        Simple { count = n + 1 ; bindings = SMap.add name (n + dis + 1) layout.bindings }
    | Reserved layout ->
        Reserved (aux (dis + 1) layout)
  in
  aux 0 layout

let idx name layout =
  let rec aux = function
    | Simple layout ->
        let n = SMap.find name layout.bindings in
        layout.count - n
    | Reserved layout ->
        aux layout
  in
  aux layout

type case = Terms.lpattern list * Terms.lterm

let specialize_match (cases : case list) =
  let aux (acc, def) = function
    | ((_, Terms.ConstPat cst) :: rest, action) ->
        begin match List.assoc_opt cst acc with
          | Some cases -> cases := (rest, action) :: !cases ; acc, def
          | None -> (cst, ref [ (rest, action) ]) :: acc, def
        end
    | ((_, Terms.WildPat) :: rest, action) ->
        List.iter (fun (_, cases) -> cases := (rest, action) :: !cases) acc ;
        acc, (rest, action) :: def
    | _ -> failwith "invalid pattern"
  in
  let spe, def = List.fold_left aux ([], []) cases in
  let spe = List.rev spe in
  let def = List.rev def in
  List.map (fun (cst, sub) -> (cst, List.rev !sub)) spe, def

let unpack_tuple (cases : case list) =
  let aux = function
    | ((_, Terms.TuplePat ps) :: rest, action) ->
        ps @ rest, action
    | _ -> failwith "invalid pattern"
  in
  List.map aux cases

type decision_tree =
  | DLeaf   of Terms.lterm
  | DSwitch of code * (Terms.const_term * decision_tree) list * decision_tree
  | DFail

let rec remove i = function
  | [] -> failwith "empty list"
  | x :: xs when i = 0 -> x, xs
  | x :: xs -> let y, ys = remove (i - 1) xs in y, x :: ys

let swap i l = let x, xs = remove i l in x :: xs

let is_signature : Terms.const_term list -> bool = function
  | Unit :: _ -> true
  | (Bool x) :: rest -> List.mem (Terms.Bool (not x)) rest
  | _ -> false

let rec codegen_match paths cases =
  match paths, cases with
    | _, [] -> DFail
    | _, ([], action) :: _ -> DLeaf action
    | _, ((_, Terms.WildPat) :: _ as row, action) :: _ ->
        begin match List.find_index (fun (_, p) -> p <> Terms.WildPat) row with
          | Some i ->
              let cases = List.map (fun (row, action) -> (swap i row, action)) cases in
              let paths = swap i paths in
              codegen_match paths cases
          | None -> DLeaf action
        end
    | path :: paths, ((_, Terms.ConstPat _) :: _, _) :: _ ->
        let cases, def = specialize_match cases in
        let cases = List.map (fun (cst, sub) -> (cst, codegen_match paths sub)) cases in
        if is_signature (List.map fst cases) then
          let (_, def), cases = remove (List.length cases - 1) cases in
          DSwitch (path, cases, def)
        else
          DSwitch (path, cases, codegen_match paths def)
    | path :: paths, ((_, Terms.TuplePat ps) :: _, _) :: _ ->
        let n = List.length ps in
        let cases = List.map (fun[@ocaml.warning "-8"] ((off, p) :: rest, action as row) ->
          if p = Terms.WildPat then
            let p = List.init n (fun _ -> (off, Terms.WildPat)) in
            ((off, Terms.TuplePat p) :: rest, action)
          else
            row
        ) cases in
        let path = List.init n (fun k -> Access (k, path)) in
        let cases = unpack_tuple cases in
        codegen_match (path @ paths) cases
    | _ -> failwith "invalid match"

let const_to_value : Terms.const_term -> value = function
  | Unit -> Unit
  | Bool b -> Bool b
  | Int k -> Int k

let codegen layout (item : Terms.toplevel_item) =
  let rec codegen_dtree layout = function
    | DFail -> Fail "match failure"
    | DLeaf action -> aux layout action
    | DSwitch (path, cases, def) ->
        List.fold_right (fun (cst, dt) acc ->
          let ifbr = codegen_dtree layout dt in
          begin match (cst : Terms.const_term) with
            | Bool true ->
                Branch { cond = path ; ifbr ; elsebr = acc }
            | Bool false ->
                Branch { cond = path ; ifbr = acc ; elsebr = ifbr }
            | cst ->
                let cond = Op (Terms.Eq, path, Load (const_to_value cst)) in
                Branch { cond ; ifbr ; elsebr = acc }
          end
        ) cases (codegen_dtree layout def)
  and aux layout (term : Terms.lterm) =
    match term with
      | (_, Const x) ->
          Load (const_to_value x)
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
      | (_, Tuple ms) ->
          MakeBlock (List.map (aux layout) ms)
      | (_, Match { expr; arms }) ->
          let dt = codegen_match [Var 0] (List.map (fun (p, m) -> [ p ], m) arms) in
          let fn = Abs (codegen_dtree (Reserved layout) dt) in
          App (fn, aux layout expr)
      | (_, IfElse { cond ; ifbr ; elsebr }) ->
          let cond = aux layout cond in
          let ifbr = aux layout ifbr in
          let elsebr = aux layout elsebr in
          Branch { cond ; ifbr ; elsebr }
  in
  match item with
    | LetDef (Some name, term) ->
        let code = aux layout term in
        (code, bind name layout)
    | LetDef (None, term) ->
        let code = aux layout term in
        (code, layout)
