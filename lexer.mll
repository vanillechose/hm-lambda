{
  type token =
    | And
    | Dot
    | Eof
    | Eq
    | False
    | Ident of string
    | In
    | LParen
    | Lambda
    | Let
    | Neq
    | Or
    | Other of char
    | RParen
    | True

  let string_of_token = function
    | And       -> "'&&'"
    | Dot       -> "'.'"
    | Eof       -> "end of file"
    | Eq        -> "'='"
    | False     -> "'#f'"
    | Ident _   -> "identifier"
    | In        -> "'in'"
    | Or        -> "'||'"
    | Other c   -> "character '" ^ (String.make 1 c) ^ "'"
    | LParen    -> "'('"
    | Lambda    -> "'\\'"
    | Let       -> "'let'"
    | Neq       -> "'<>'"
    | RParen    -> "')'"
    | True      -> "'#t'"

  let make_keyword_or_ident = function
    | "let" -> Let
    | "in"  -> In
    | w     -> Ident w
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
