{
  type token =
    | And
    | Dot
    | Else
    | Eof
    | Eq
    | False
    | Ident of string
    | If
    | In
    | LParen
    | Lambda
    | Let
    | Neq
    | Or
    | Other of char
    | RParen
    | Then
    | True

  let string_of_token = function
    | And       -> "'&&'"
    | Dot       -> "'.'"
    | Else      -> "'else'"
    | Eof       -> "end of file"
    | Eq        -> "'='"
    | False     -> "'#f'"
    | Ident _   -> "identifier"
    | If        -> "'if'"
    | In        -> "'in'"
    | LParen    -> "'('"
    | Lambda    -> "'\\'"
    | Let       -> "'let'"
    | Neq       -> "'<>'"
    | Or        -> "'||'"
    | Other c   -> "character '" ^ (String.make 1 c) ^ "'"
    | RParen    -> "')'"
    | Then      -> "'then'"
    | True      -> "'#t'"

  let make_keyword_or_ident = function
    | "else"  -> Else
    | "if"    -> If
    | "in"    -> In
    | "let"   -> Let
    | "then"  -> Then
    | w       -> Ident w
}

let ws     = [' ' '\t' '\n']
let istart = ['a'-'z' 'A'-'Z' '_']
let icont  = ['a'-'z' 'A'-'Z' '0'-'9' '_']

rule token = parse
  | ws+  { token lexbuf }
  | "#f" { False        }
  | "#t" { True         }
  | "&&" { And          }
  | "<>" { Neq          }
  | "||" { Or           }
  | '('  { LParen       }
  | ')'  { RParen       }
  | '.'  { Dot          }
  | '='  { Eq           }
  | '\\' { Lambda       }
  | (istart icont*) as w
         { make_keyword_or_ident w }
  | eof
         { Eof          }
  | _ as c
         { Other c      }
