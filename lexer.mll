{
  exception Syntax_error of string

  let reserved_words = [
    ("if", Parser.IF);
    ("else", Parser.ELSE);
    ("for", Parser.FOR);
    ("while", Parser.WHILE);
    ("or", Parser.OR);
    ("and", Parser.AND);
    ("func", Parser.FUNC);
    ("local", Parser.LOCAL);
    ("true", Parser.TRUE);
    ("false", Parser.FALSE);
    ("return", Parser.RETURN);
  ]
}

let int = '0' | (['1'-'9']['0'-'9']*)
let identifier = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | [' ' '\009' '\012' '\n']+ { read lexbuf }
  | "==" { Parser.EQ }
  | "!=" { Parser.NE }
  | "<=" { Parser.LE }
  | ">=" { Parser.GE }
  | '<' { Parser.LT }
  | '>' { Parser.GT }
  | '=' { Parser.ASSIGN }
  | '(' { Parser.LPAREN }
  | ')' { Parser.RPAREN }
  | '{' { Parser.LBRACE }
  | '}' { Parser.RBRACE }
  | '+' { Parser.PLUS }
  | '-' { Parser.MINUS }
  | '*' { Parser.MULT }
  | '/' { Parser.DIV }
  | ';' { Parser.SEMI }
  | ',' { Parser.COMMA }
  | eof { Parser.EOF }
  | int { Parser.NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | identifier
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reserved_words
      with
      _ -> Parser.IDENTIFIER (id)
    }
  | _ { raise (Syntax_error ("Unexpected char: " ^ (Lexing.lexeme lexbuf))) }
  