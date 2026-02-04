(* type variable *)
type var = Bound of int | Free of int

type const =
  | UnitType
  | BoolType
  | IntType

(* type or type scheme *)
type typ =
  | ConstType of const
  | VarType   of var
  | ArrType   of typ * typ
  | TupleType of typ list

module Info = struct
  type t = typ

  open Termfmt

  let term_info = function
    | ConstType UnitType -> Constant "unit"
    | ConstType BoolType -> Constant "bool"
    | ConstType IntType -> Constant "int"
    | VarType (Bound x) -> Constant ("'" ^ string_of_int x)
    | VarType (Free x) -> Constant ("'_" ^ string_of_int x)
    | ArrType (a, b) ->
        Binary { prec = 1 ; assoc = Right ; left = a ;
          infix = " -> " ; right = b }
    | TupleType (_ :: [] | []) -> failwith "tuple with < 2 elements"
    | TupleType (a :: bs) ->
        Nary { prec = 2 ; prefix = "" ; head = a ;
          suffix = "" ; children = List.map (fun b -> (" * ", b)) bs }
end

module F = Termfmt.Make(Info)

let string_of_type = F.string_of_t

module IMap = Map.Make(Int)
module ISet = Set.Make(Int)
module SMap = Map.Make(String)

type substitution = (int * typ) list

(* occurs-check *)
let rec occurs x = function
  | ConstType _ -> false
  | VarType (Bound _) -> false
  | VarType (Free x') -> x = x'
  | ArrType (a, b) -> occurs x a || occurs x b
  | TupleType bs -> List.exists (occurs x) bs

(* applies substitution r to type a. *)
let subst r a =
  let rec aux = function
    | ConstType _ as c -> c
    (* treat bound variables as constants *)
    | VarType (Bound _) as a -> a
    | VarType (Free x) ->
        begin match List.assoc_opt x r with
          | Some a -> a
          | None -> VarType (Free x)
        end
    | ArrType (a, b) -> ArrType (aux a, aux b)
    | TupleType bs -> TupleType (List.map aux bs)
  in
  aux a

(* computes subsitution r . s *)
let compose r s =
  List.map (fun (x, a) -> (x, subst r a)) s @ r
  
exception Unify

let rec mgu a b : substitution =
  match a, b with
    | (VarType (Free x), VarType (Free y)) ->
        if x = y then [] else [ (x, b) ]
    | (VarType (Free x), a)
    | (a, VarType (Free x)) ->
        if occurs x a then
          raise Unify
        else
          [ (x, a) ]
    | (ArrType(a, b), ArrType(a', b')) ->
        let r = mgu a a' in
        let s = mgu (subst r b) (subst r b') in
        compose r s
    | (TupleType bs, TupleType bs') ->
        if List.length bs <> List.length bs' then
          raise Unify ;
        List.fold_left (fun r (a, a') ->
          let s = mgu (subst r a) (subst r a') in
          compose r s
        ) [] (List.combine bs bs')
    | (ConstType c, ConstType c') ->
        if c = c' then [] else raise Unify
    | (_, VarType (Bound _))
    | (VarType (Bound _), _) ->
        failwith "bound variables should not appear in instantiated types"
    | _ ->
        raise Unify

type env = typ SMap.t

let empty_env = SMap.empty

let free_vars_of_type a =
  let rec aux acc = function
    | ConstType _ -> acc
    | VarType (Free x) -> ISet.add x acc
    | VarType (Bound _) -> acc
    | ArrType (a, b) -> aux (aux acc a) b
    | TupleType bs -> List.fold_left aux acc bs
  in
  aux ISet.empty a

let free_vars_of_env env =
  SMap.fold (fun _ a acc -> ISet.union (free_vars_of_type a) acc) env ISet.empty

let newvarname, reset_newvar =
  let i = ref 0 in
  ((fun () -> let x = !i in i := x + 1 ; x),
   (fun env -> i := 1 + try ISet.max_elt (free_vars_of_env env) with _ -> -1))

let newvar () =
  let x = newvarname () in
  VarType (Free x)

let generalize env a =
  let fv = free_vars_of_env env in
  let rec aux = function
    | ConstType _ as c -> c
    | VarType (Free x) ->
        if ISet.mem x fv then
          VarType (Free x)
        else
          VarType (Bound x)
    | VarType (Bound _) -> failwith "attempt to generalize a type scheme"
    | ArrType (a, b) -> ArrType(aux a, aux b)
    | TupleType bs -> TupleType (List.map aux bs)
  in
  aux a

let instantiate a =
  let newvars = ref IMap.empty in
  let rec aux = function
    | ArrType (a, b) -> ArrType (aux a, aux b)
    | TupleType bs -> TupleType (List.map aux bs)
    | VarType (Bound x) ->
        begin match IMap.find_opt x !newvars with
          | Some y -> VarType (Free y)
          | None ->
              let y = newvarname () in
              newvars := IMap.add x y !newvars ;
              VarType (Free y)
        end
    | a -> a
  in
  aux a

let bind name a env =
  SMap.add name a env

let bind_let name a env =
  bind name (generalize env a) env

let subst_env r env =
  SMap.map (fun a -> subst r a) env

open Terms

type semantic_error =
  | UnboundVariable of location * string
  | TypeMismatch    of { loc : location ; expected : typ ; found : typ }
  | TypePatMismatch of { loc : location ; expected : typ ; found : typ }

exception Sem_error of semantic_error

let type_constant_term = function
  | Bool _ -> ConstType BoolType
  | Unit -> ConstType UnitType
  | Int _ -> ConstType IntType

let rec type_pattern newvar = function
  | (_, TuplePat ps) -> TupleType (List.map (type_pattern newvar) ps)
  | (_, ConstPat c) -> type_constant_term c
  | (_, WildPat) -> newvar ()

(* Type a λ-term using Hindley-Milner type inference (algorithm W) *)
let type_term env term =
  reset_newvar env ;
  let rec expect_type env (loc, _ as term) expected =
    let rm, a = w env term in
    let exp   = subst rm expected in
    let v =
      try mgu a exp with
        | Unify ->
            let err = TypeMismatch { loc ; expected ; found = a } in
            raise (Sem_error err)
    in
    compose v rm
  and type_term_list (acc : substitution * typ list) env = function
    | [] -> (fst acc, List.rev (snd acc))
    | m :: ms ->
        let rm, a = w env m in
        let env   = subst_env rm env in
        let r     = compose rm (fst acc) in
        let bs    = List.map (subst rm) (snd acc) in
        type_term_list (r, a :: bs) env ms
  and type_match_cases env acc type_pat type_arm = function
    | ((loc, _ as pat), m) :: rest ->
        let type_pat' = type_pattern newvar pat in
        let rp =
          try mgu type_pat type_pat' with
            | Unify ->
                raise (Sem_error
                  (TypePatMismatch { loc ; expected = type_pat ; found = type_pat' }))
        in
        let env = subst_env rp env in
        let type_arm = subst rp type_arm in
        let rm = expect_type env m type_arm in
        type_match_cases
          (subst_env rm env)
          (compose rm (compose rp acc))
          (subst rm type_pat')
          (subst rm type_arm)
          rest
    | [] -> (acc, type_arm)

  and w env (loc, term) =
    match term with
      | Const c -> ([], type_constant_term c)
      | BinOp ((And | Or), m, n) ->
          let rm  = expect_type env m (ConstType BoolType) in
          let env = subst_env rm env in
          let rn  = expect_type env n (ConstType BoolType) in
          (compose rn rm, ConstType BoolType)
      | BinOp ((Eq | Neq), m, n) ->
          let rm, a = w env m in
          let env   = subst_env rm env in
          let rn    = expect_type env n a in
          (compose rn rm, ConstType BoolType)
      | Tuple ms ->
          let r, bs = type_term_list ([], []) env ms in
          (r, TupleType bs)
      | IfElse { cond ; ifbr ; elsebr } ->
          let rm    = expect_type env cond (ConstType BoolType) in
          let env   = subst_env rm env in
          let rn, a = w env ifbr in
          let env   = subst_env rn env in
          let ro    = expect_type env elsebr a in
          (compose ro (compose rn rm), subst ro a)
      | Match { expr = m ; arms } ->
          let rm, type_pat = w env m in
          let type_arm = newvar () in
          let rn, type_arm =
            type_match_cases
              (subst_env rm env)
              []
              type_pat
              type_arm
              arms
          in
          (compose rn rm, type_arm)
      (* (i) variable *)
      | Var x ->
          begin match SMap.find_opt x env with
            | Some a -> ([], instantiate a)
            | None -> raise (Sem_error (UnboundVariable (loc, x)))
          end
      (* (iii) abstractions *)
      | Abs (x, m) ->
          let a    = newvar () in
          let env  = bind x a env in
          let r, b = w env m in
          (r, ArrType (subst r a, b))
      (* (ii) applications *)
      | App ((loc, _ as m), n) ->
          let rm, a = w env m in
          let env   = subst_env rm env in
          let rn, b = w env n in
          let c     = newvar () in
          let arrbc = ArrType (b, c) in
          (* solve rn(a) ?= b -> c *)
          let v =
            try mgu (subst rn a) arrbc with 
              | Unify ->
                  let err = TypeMismatch { loc ; expected = arrbc ; found = a } in
                  raise (Sem_error err)
          in
          (compose v (compose rn rm), subst v c)
      (* (iv) let bindings *)
      | LetIn (x, m, n) ->
          let rm, a = w env m in
          let env   = subst_env rm env in
          let env   = bind_let x a env in
          let rn, b = w env n in
          (compose rn rm, b)
  in
  try Ok (w env term) with Sem_error e -> Error e

let type_expression env term =
  match type_term env term with
    | Ok (r, a) -> let env = subst_env r env in Ok (a, env)
    | Error e -> Error e

let type_item env = function
  | LetDef (Some name, term) ->
      let r = type_expression env term in
      Result.map (fun (typ, env) ->
        let env = bind_let name typ env in
        (SMap.find name env, env)
      ) r
  | LetDef (None, term) ->
      let r = type_expression env term in
      Result.map (fun (typ, env) -> (generalize env typ, env)) r

