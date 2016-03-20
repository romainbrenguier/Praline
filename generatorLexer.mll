{
  open GeneratorParser
  exception Eof

  let nb_line = ref 1
}

rule token = parse
    [' ' '\t']   { token lexbuf }
  | '\n'   { incr nb_line; token lexbuf }
  | "/*" { comments 0 lexbuf }
  | "//" { line_comments lexbuf }
  | ('-'? ['0'-'9']+ '.' ['0'-'9']*) as lxm { FLOAT(float_of_string lxm) }
  | ('-'? ['0'-'9']+) as lxm { INT(int_of_string lxm) }

  | "int"            { INTD }
  | "const"            { CST }
  | "player"            { PLAYER }

  | "legal"            { LEGAL }
  | "action"            { ACTION }
  | "move"            { MOVE }
  | "update"            { UPDATE }

  | "true" {TRUE}
  | "false" {FALSE}
  | "=="            { EQUALS }
  | "!="            { UNEQUALS }
  | "<"            { SMALLER }
  | "<="            { SMALLEREQ }
  | ">"            { GREATER }
  | ">="            { GREATEREQ }

  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | "atan"            { ATAN }
  | "exp"            { EXP }

  | "not"            { NOT }
  | "&&"            { AND }
  | "/\\"            { AND }
  | "\\/"            { OR }
  | "||"            { OR }

  | "if"            { IF }
  | "else"            { ELSE }
  | "while"            { WHILE }


  | "eventual" { EVENTUAL } 
  | "repeated" { REPEATED } 
  | "safety" { SAFETY } 
  | "liveness" { LIVENESS } 

  | "="            { SET }

  | ';'            { SEMI }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACK }
  | '}'            { RBRACK }

  | ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { IDENT(lxm) }
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { UIDENT(lxm) }

  | eof            { EOF }
  | _ as c { Printf.printf "unexpected char %c\n" c; token lexbuf }


and comments level = parse
  | "*/" { if level = 0 then token lexbuf
	   else comments (level-1) lexbuf
	 }
  | "/*" { comments (level+1) lexbuf }
  | '\n'   { incr nb_line; comments level lexbuf }
  | _ { comments level lexbuf }
  | eof { prerr_endline "warning: unclosed comment";
	  EOF }
and line_comments = parse
  | '\n'   { incr nb_line; token lexbuf }
  | _ { line_comments lexbuf }
  | eof { EOF }
