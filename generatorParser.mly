%{

  open Generator

  let parse_error s =
    print_endline s

  let constants = Hashtbl.create 10
  let find_constant x = 
    try Hashtbl.find constants x
    with Not_found -> failwith ("constant "^x^" not found")

  let set_constant x i =
    Hashtbl.replace constants x i

  type declaration = 
    | Var of string * int
    | Player of string * Expression.t

  type objective = 
    | MaxF | MaxG | MaxFG | MaxGF
      
%}

%token EOF
%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token <string> UIDENT
%token ACTION
%token INTD CST PLAYER LEGAL MOVE UPDATE
%token TRUE FALSE EQUALS UNEQUALS GREATER GREATEREQ SMALLER SMALLEREQ
%token AND OR NOT
%token EXISTS FORALL 
%token EVENTUAL REPEATED SAFETY LIVENESS
%token SET IF ELSE WHILE
%token SEMI LPAREN RPAREN
%token LBRACK RBRACK
%token PLUS MINUS DIV TIMES ATAN EXP

%left PLUS MINUS        /* lowest precedence */
%left AND OR        /* lowest precedence */
%left NOT         /* medium precedence */
%left DIV TIMES
%nonassoc EQUALS UNEQUALS

%start main

%type <(Generator.Generic.t * Generator.Generic.G.t)> main

%start instruction_list
%type <(Generator.Instruction.t)> instruction_list
%%


int :
  | INT { $1 }
  | UIDENT { find_constant $1 }
;

value : 
  | int   { Expression.Int $1 }
  | FLOAT   { Expression.Float $1 }
  | IDENT { Expression.Var $1 }
  | ACTION IDENT { Expression.Action (Player.of_string $2) }
;

expr:
  | value {Expression.Val $1}
  | LPAREN expr RPAREN  { $2 }
  | expr EQUALS expr  { Expression.Apply("==",[$1;$3]) }
  | expr UNEQUALS expr  { Expression.Apply("!=",[$1;$3]) }
  | expr GREATER expr { Expression.Apply(">",[$1;$3]) }
  | expr SMALLER expr { Expression.Apply("<",[$1;$3]) }
  | expr GREATEREQ expr { Expression.Apply(">=",[$1;$3]) }
  | expr SMALLEREQ expr { Expression.Apply("<=",[$1;$3]) }
  | expr PLUS expr { Expression.Apply("+",[$1;$3]) }
  | expr MINUS expr { Expression.Apply("-",[$1;$3]) }
  | expr TIMES expr { Expression.Apply("*",[$1;$3]) }
  | expr DIV expr { Expression.Apply("/",[$1;$3]) }
  | EXP expr { Expression.Apply("exp",[$2]) }
  | expr AND expr { Expression.Apply("and",[$1;$3]) }
  | expr OR expr { Expression.Apply("or",[$1;$3]) }
  | NOT expr { Expression.Apply("not",[$2]) }
;

expr_list:
  | expr_list expr { $2 :: $1 }
  | expr {[$1]}
;


instruction:  
  | WHILE LPAREN expr RPAREN instruction 
      { Instruction.While ($3,$5) }
  | IF LPAREN expr RPAREN instruction ELSE instruction
      { Instruction.If ($3,$5,$7) }
  | IF LPAREN expr RPAREN instruction { Instruction.If ($3,$5,Instruction.Seq[]) }
  | IDENT SET expr SEMI { Instruction.Set ($1, $3)}
  | LEGAL IDENT expr_list SEMI { Instruction.Legal ($2, $3)}
  | LBRACK instruction_list RBRACK {$2}
  | LBRACK RBRACK { Instruction.Seq [] }
; 

instruction_list:
  | instruction_list instruction { Instruction.Seq [$1;$2] }
  | instruction {$1}
;


declaration:
  | PLAYER IDENT LBRACK expr RBRACK SEMI
      { Player ($2, $4) }
  | INTD IDENT SET int SEMI
      { Var ($2,$4) }
  | INTD IDENT SEMI
      { Var ($2,0) }
;

constant_declaration:
  | CST UIDENT SET int SEMI { set_constant $2 $4}
;

declaration_list:
  | declaration_list constant_declaration { $1 }
  | declaration_list declaration { $2 :: $1}
  | declaration { [$1] }
  | constant_declaration { [] }
;

local:
  | INTD IDENT SET int SEMI { ($2,$4) }
  | INTD IDENT SEMI    { ($2,0) }
;

local_list:
  | local_list local { $2 :: $1}
  | local { [$1] }
;

move:
  | MOVE LBRACK local_list instruction_list RBRACK { ($3,$4) }
  | MOVE LBRACK instruction_list RBRACK { ([],$3) }
;

update:
  | UPDATE LBRACK local_list instruction_list RBRACK { ($3,$4) }
  | UPDATE LBRACK instruction_list RBRACK { ([],$3) }
;

main:
  declaration_list 
  move 
  update EOF 
  {
    let rec loop (env,obj) = function
      | [] -> (env,obj)
      | Var (ident,init) :: s ->
	let new_env = StringMap.add ident init env in
	loop (new_env,obj) s
      | Player (name,expr) :: s ->
	let new_obj = StringMap.add name expr obj in
	loop (env,new_obj) s

    in
    let init,obj = loop (StringMap.empty,StringMap.empty) (List.rev $1) in
    let players = StringMap.fold (fun p _ accu -> Player.of_string p :: accu) obj [] in 
    let payoff p s = 
      let player = Player.to_string p in
      let expr = StringMap.find player obj in 
      (* !! we have to be carefull about the precision *)
      int_of_float (Expression.evaluate_float s Move.empty expr)
    in
    
    let local,instr_move = $2 in
    let move s = 
      let s_with_local = 
	List.fold_left
	  (fun accu (x,v) -> StringMap.add x v accu) s local
      in
      let allowed = Instruction.legal s_with_local instr_move in
      fun player -> 
	let p = Player.to_string player in
	try 
	  StringMap.find p allowed 
	with Not_found -> 
	  Printf.eprintf "warning : no action allowed for %s in state %s\n"
	    p (Parser.string_of_state s); []
	    
    in
    
    let local,instr = $3 in
    let tab s m = 
      let s_with_local = 
	List.fold_left
	  (fun accu (x,v) -> StringMap.add x v accu) s local
      in
      let res_with_local = Instruction.evaluate s_with_local m instr in
      List.fold_left (fun accu (x,_) -> StringMap.remove x accu) res_with_local local
    in
    
    let desc = 
      { 
	Generic.players = players; 
	payoff = payoff; 
	init = init;
	move = move;
	tab = tab;
      }
    in 
    desc, Generic.game desc

  }
;

