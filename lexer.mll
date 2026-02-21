{
  type token =
    | And
    | Arrow
    | Case
    | Comma
    | Dot
    | Else
    | Eof
    | Eq
    | False
    | Ident of string
    | If
    | In
    | Integer of int
    | LParen
    | Lambda
    | Let
    | Match
    | Minus
    | Neq
    | Or
    | Other of char
    | Plus
    | Rec
    | RParen
    | Semisemi
    | Then
    | Times
    | True
    | Wildcard
    | With

  let string_of_token = function
    | And       -> "'&&'"
    | Arrow     -> "'->'"
    | Case      -> "'case'"
    | Comma     -> "','"
    | Dot       -> "'.'"
    | Else      -> "'else'"
    | Eof       -> "end of file"
    | Eq        -> "'='"
    | False     -> "'#f'"
    | Ident _   -> "identifier"
    | If        -> "'if'"
    | In        -> "'in'"
    | Integer k -> "'" ^ string_of_int k ^ "'"
    | LParen    -> "'('"
    | Lambda    -> "'\\'"
    | Let       -> "'let'"
    | Match     -> "'match'"
    | Minus     -> "'-'"
    | Neq       -> "'<>'"
    | Or        -> "'||'"
    | Other c   -> "character '" ^ (String.make 1 c) ^ "'"
    | Plus      -> "'+'"
    | RParen    -> "')'"
    | Rec       -> "'rec'"
    | Semisemi  -> "';;'"
    | Then      -> "'then'"
    | Times     -> "'*'"
    | True      -> "'#t'"
    | Wildcard  -> "'_'"
    | With      -> "'with'"

  let make_keyword_or_ident = function
    | "case"  -> Case
    | "else"  -> Else
    | "if"    -> If
    | "in"    -> In
    | "let"   -> Let
    | "match" -> Match
    | "rec"   -> Rec
    | "then"  -> Then
    | "with"  -> With
    | "_"     -> Wildcard
    | w       -> Ident w
}

let ws     = [' ' '\t' '\n']
let istart = ['a'-'z' 'A'-'Z' '_']
let icont  = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let digits = ['0'-'9']

rule token = parse
  | "#f" { False        }
  | "#t" { True         }
  | "&&" { And          }
  | "->" { Arrow        }
  | ";;" { Semisemi     }
  | "<>" { Neq          }
  | "||" { Or           }
  | '('  { LParen       }
  | ')'  { RParen       }
  | '*'  { Times        }
  | '+'  { Plus         }
  | ','  { Comma        }
  | '-'  { Minus        }
  | '.'  { Dot          }
  | '='  { Eq           }
  | '\\' { Lambda       }
  | ws+  { token lexbuf }
  | (digits+) as w
         { Integer (int_of_string w) }
  | (istart icont*) as w
         { make_keyword_or_ident w }
  | eof
         { Eof          }
  | _ as c
         { Other c      }
