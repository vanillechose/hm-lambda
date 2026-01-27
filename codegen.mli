(* layout of the execution environment *)
type layout

val empty_layout : layout

(* Turns a well-typed λ-term into its representation using
 * de Bruijn indices. Also turns let x = M in N into (\. N) M *)
val codegen : layout -> Terms.toplevel_item -> (Eval.code * layout)
