(* type or type scheme *)
type typ

val string_of_type : typ -> string

(* typing context *)
type env

val empty_env : env

type semantic_error =
  | UnboundVariable  of Terms.location * string
  | TypeMismatch     of { loc : Terms.location ; expected : typ ; found : typ }
  | TypePatMismatch  of { loc : Terms.location ; expected : typ ; found : typ }
  | DuplicateBinding of { loc : Terms.location ; name : string ; pattern : Terms.lpattern }

(* type a toplevel item and returns its type and the updated typing context *)
val type_item : env -> Terms.toplevel_item -> (typ * env, semantic_error) result
