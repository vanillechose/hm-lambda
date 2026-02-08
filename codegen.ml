open Eval
open Terms

module SMap = Map.Make(String)

(* - count is the number of bindings in the environment ;
 * - binding maps a name to its index starting from the end of the environment.
 * - Reserved L is used to reserve a slot in L without binding a name *)
type layout = { count : int ; bindings : int SMap.t }

let empty_layout = { count = 0 ; bindings = SMap.empty }

let bind name layout =
  let n = layout.count in
  { count = n + 1 ; bindings = SMap.add name (n + 1) layout.bindings }

let reserve layout =
  { layout with count = layout.count + 1 }

let idx name layout =
  let n = SMap.find name layout.bindings in
  layout.count - n

(* === pattern matching to decision trees === *)
type action = lterm
type row    = lpattern list * action
type matrix = row list

type decision_tree =
  | Leaf   of action
  | Switch of code * (atomic_val * decision_tree) list * decision_tree
  | Fail

(* pattern matrices manipulation *)
let rec remove i = function
  | [] -> failwith "empty list"
  | x :: xs when i = 0 -> x, xs
  | x :: xs -> let y, ys = remove (i - 1) xs in y, x :: ys

let swap i l = let x, xs = remove i l in x :: xs

(* swap a column i with the first column of the matrix *)
let select_column i paths matrix =
  let paths = swap i paths in
  let matrix = List.map (fun (ps, act) -> (swap i ps, act)) matrix in
  (paths, matrix)

let find_first_non_wildcard : matrix -> int option = function
  | [] -> None
  | (ps, _) :: _ -> List.find_index (fun (_, p) -> p <> Pwildpat) ps

let specialize_by_constant (matrix : matrix) =
  let append_row cst ps act cases =
    match List.assoc_opt cst !cases with
      | Some rows -> rows := (ps, act) :: !rows
      | None -> cases := (cst, ref [ ps, act ]) :: !cases
  in

  let rec fold rows cases default =
    match rows with
      | [] -> (cases, default)
      | (((_, p) :: rest), act) :: rows' ->
          begin match p with
            | Pconstpat cst ->
                append_row cst rest act cases ;
                fold rows' cases default
            | Pwildpat ->
                List.iter (fun (_, rows) -> rows := (rest, act) :: !rows) !cases ;
                fold rows' cases ((rest, act) :: default)
            | _ -> failwith "invalid pattern (no constructor in column)"
          end
      | _ -> failwith "invalid pattern (empty row)"
  in

  let cases, default = fold matrix (ref []) [] in
  let cases = List.map (fun (cst, rows) -> (cst, List.rev !rows)) !cases in
  let default = List.rev default in
  (cases, default)

let specialize_tuple arity (matrix : matrix) =
  List.map (fun (ps, action) ->
    match ps with
      | (_, Ptuplepat ps) :: rest ->
          (ps @ rest, action)
      | (off, Pwildpat) :: rest ->
          let ps = List.init arity (fun _ -> (off, Pwildpat)) in
          (ps @ rest, action)
      | _ -> failwith "invalid pattern (tuple unpacking)"
  ) matrix

let is_signature : atomic_val list -> bool = function
  | Aunit :: _ -> true
  | Abool b :: rest -> List.mem (Abool (not b)) rest
  | _ -> false

let rec compile_matrix paths matrix =
  match matrix with
    | [] -> Fail
    | ([], act) :: _ -> Leaf act
    | _ ->
        match find_first_non_wildcard matrix with
          | Some i when i > 0 ->
              let paths, matrix = select_column i paths matrix in
              compile_selected_pattern paths matrix
          | _ -> compile_selected_pattern paths matrix

and compile_selected_pattern paths matrix =
  match paths, matrix with
    | path :: paths, ((_, Ptuplepat ps) :: _, _) :: _ ->
        let arity  = List.length ps in
        let paths  = List.init arity (fun k -> Access (k, path)) @ paths in
        let matrix = specialize_tuple arity matrix in
        compile_matrix paths matrix
    | path :: paths, ((_, Pconstpat _) :: _, _) :: _ ->
        let cases, default = specialize_by_constant matrix in
        let compiled_cases =
          List.map (fun (cst, matrix) -> (cst, compile_matrix paths matrix)) cases
        in
        (* if the set of pattern in the first row form a signature, the default
         * matrix is not needed, it is safe to replace it with the last case *)
        if is_signature (List.map fst cases) then
          let (_, default), compiled_cases = remove (List.length cases - 1) compiled_cases in
          Switch (path, compiled_cases, default)
        else
          let compiled_default = compile_matrix paths default in
          Switch (path, compiled_cases, compiled_default)
    | _ :: _, ((_, Pwildpat) :: _, act) :: _ ->
        (* the whole line is filled with wildcards *)
        Leaf act
    | _ -> failwith "invalid pattern (selected column doesn't match any of the cases)"

(* === actual code generation === *)

let quote_atomic_value = function
  | Aunit   -> Quote Unit
  | Abool x -> Quote (Bool x)
  | Aint k  -> Quote (Int k)

let rec codegen_term layout (_, term) =
  match term with
    | Pconst x -> quote_atomic_value x
    | Pvar x -> Var (idx x layout)
    | Pprim (f, m, n) -> Op (f, codegen_term layout m, codegen_term layout n)
    | Plambda (x, m) -> let m = codegen_term (bind x layout) m in Abs m
    | Papp (m, n) -> App (codegen_term layout m, codegen_term layout n)
    (* let x = M in N ~> (\x. N) M *)
    | Pletin (x, m, n) -> App (Abs (codegen_term (bind x layout) n), codegen_term layout m)
    | Ptuple ms -> MakeBlock (List.map (codegen_term layout) ms)
    | Pmatch { expr ; arms } ->
        let matrix = List.map (fun (p, m) -> [ p ], m) arms in
        let dt = compile_matrix [ Var 0 ] matrix in
        let fn = Abs (codegen_decision_tree (reserve layout) dt) in
        App (fn, codegen_term layout expr)
    | Pifelse { cond ; ifbr ; elsebr } ->
        let cond = codegen_term layout cond in
        let ifbr = codegen_term layout ifbr in
        let elsebr = codegen_term layout elsebr in
        Branch { cond ; ifbr ; elsebr }

and codegen_decision_tree layout = function
  | Fail -> Fail "match failure"
  | Leaf action -> codegen_term layout action
  | Switch (path, cases, def) ->
      List.fold_right (fun (cst, dt) acc ->
        let ifbr = codegen_decision_tree layout dt in
        match cst with
          (* Avoid if x = #t... tests *)
          | Abool v ->
              Branch { cond = path ;
                ifbr = if v then ifbr else acc ;
                elsebr = if v then acc else ifbr }
          | _ ->
              let cond = Op (Oeq, path, quote_atomic_value cst) in
              Branch { cond ; ifbr ; elsebr = acc }
      ) cases (codegen_decision_tree layout def)

and codegen layout item =
  match item with
    | Pletdef (Some name, term) ->
        let code = codegen_term layout term in
        (code, bind name layout)
    | Pletdef (None, term) ->
        let code = codegen_term layout term in
        (code, layout)
