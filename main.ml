let extract_line source loc =
  match
    String.rindex_from_opt source loc '\n',
    String.index_from_opt source loc '\n'
  with
    | None, None -> (0, String.length source)
    | Some lo, None -> (lo + 1, String.length source)
    | None, Some hi -> (0, hi)
    | Some lo, Some hi -> (lo + 1, hi)

let err_with_source source loc msg =
  let lo, hi = extract_line source loc in
  let line = String.sub source lo (hi - lo) in
  let linenum =
    let lines = String.split_on_char '\n' (String.sub source 0 lo) in
    List.length lines
  in
  Printf.eprintf "error: %s\n%!" msg ;
  Printf.eprintf " %3d | %s\n%!" linenum line ;
  Printf.eprintf "     | %*s^\n%!" (loc - lo) ""

let report_parse_error source = function
  | Terms.EmptyTerm loc ->
      err_with_source source loc "empty term"
  | Terms.NoCases loc ->
      err_with_source source loc "this match expression has no arms"
  | Terms.SyntaxError (loc, msg) ->
      err_with_source source loc msg
  | Terms.UnexpectedEOF ->
      err_with_source source (String.length source - 1) "unexpected end of file"

let report_semantic_error source = function
  | Typing.UnboundVariable (loc, name) ->
      err_with_source source loc ("unbound variable " ^ name)
  | Typing.TypeMismatch { loc ; expected ; found } ->
      let msg = "this expression has type " ^ Typing.string_of_type found ^ " but type "
        ^ Typing.string_of_type expected ^ " was expected" in
      err_with_source source loc msg
  | Typing.TypePatMismatch { loc ; expected ; found } ->
      let msg = "this pattern has type " ^ Typing.string_of_type found ^ " but type "
        ^ Typing.string_of_type expected ^ " was expected" in
      err_with_source source loc msg

(* repl state *)
let toplevel_env    = ref Typing.empty_env
let codegen_layout  = ref Codegen.empty_layout
let interpreter_env = ref []
let show_tree       = ref false
let show_trace      = ref false

let print_eval_result name v typ =
  Printf.printf "%s : %s = %s\n%!" name (Typing.string_of_type typ) (Eval.string_of_value v)

let rec interp source =
  let r = Terms.parse source in
  (* multiline repl, try and read another line *)
  if r = Error UnexpectedEOF then begin
    if In_channel.isatty In_channel.stdin then
      Printf.printf "    %!" ;
    match read_line () with
      | exception End_of_file -> print_newline () ; Error ()
      | line -> interp (source ^ "\n" ^ line)
  end else
    let r = Result.map_error (report_parse_error source) r in
    Result.bind r (fun (Pletdef(name, _) as item) ->
      let r = Typing.type_item !toplevel_env item in
      let r = Result.map_error (report_semantic_error source) r in
      Result.bind r (fun (typ, env) ->
        toplevel_env := env ;
        let (code, layout) = Codegen.codegen !codegen_layout item in
        codegen_layout := layout ;

        if !show_tree then begin
          print_endline "=== TREE ===" ;
          print_endline (Eval.string_of_code code) ;
          print_endline "============" ;
        end ;

        let v = Eval.eval ~trace:!show_trace !interpreter_env code in
        Result.bind v (fun v ->
          let name =
            match name with
              | Some name -> interpreter_env := v :: !interpreter_env ; name
              | None -> "-"
          in
          print_eval_result name v typ ;
          Ok ()
        )
      )
    )

let dump_env () =
  List.iteri
    (fun n v -> Printf.printf "%d = %s\n%!" n (Eval.string_of_value v))
    !interpreter_env

let rec repl _ =
  let toggle opt s =
    opt := not !opt ;
    print_endline (s ^ " = " ^ string_of_bool !opt)
  in
  if In_channel.isatty In_channel.stdin then
    Printf.printf ">>> %!" ;
  match read_line () with
    | exception End_of_file -> print_newline ()
    | line when String.trim line = "#env" ->
        dump_env () ;
        repl ()
    | line when String.trim line = "#tree" ->
        toggle show_tree "show tree" ;
        repl ()
    | line when String.trim line = "#trace" ->
        toggle show_trace "show trace" ;
        repl ()
    | line when String.trim line = "" ->
        repl ()
    | line ->
        let _ = interp line in
        repl ()

let _ = repl ()
