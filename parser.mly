%token<string> IDENTIFIER
%token<int> NUMBER
%token COMMA SEMI
%token FUNC LOCAL RETURN
%token IF ELSE FOR WHILE
%token LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS MULT DIV
%token LE LT GE GT NE EQ OR AND
%token ASSIGN
%token TRUE FALSE
%token EOF

%left OR
%left AND
%left LT LE GT GE EQ NE
%left PLUS MINUS
%left MULT DIV

%start program
%type<Ast.program> program

%%

program
  : eds = list(external_definition); EOF
    { eds }
  ;
external_definition
  : FUNC; fn = IDENTIFIER; LPAREN; params = separated_list(COMMA, IDENTIFIER); RPAREN; body = block
    { Ast.Function_definition (fn, params, body) }
  | LOCAL; vn = IDENTIFIER; ASSIGN; e = expression; SEMI
    { Ast.Variable_definition (vn, Some e) }
  | LOCAL; vn = IDENTIFIER; SEMI
    { Ast.Variable_definition (vn, None) }
  ;
block
  : LBRACE; stmts = list(statement); RBRACE
    { stmts }
  ;
statement
  : IF; LPAREN; condition = expression; RPAREN; stmt = statement
    { Ast.If_statement (condition, stmt, None) }
  | IF; LPAREN; condition = expression; RPAREN; stmt1 = statement; ELSE; stmt2 = statement
    { Ast.If_statement (condition, stmt1, Some stmt2) }
  | FOR; LPAREN; init_stmt = statement; condition = expression; SEMI; prop_stmt = statement; RPAREN; stmt = statement
    { Ast.For_statement (init_stmt, condition, prop_stmt, stmt) }
  | WHILE; LPAREN; condition = expression; RPAREN; stmt = statement
    { Ast.While_statement (condition, stmt) }
  | stmts = block
    { Ast.Block_statement (stmts) }
  | LOCAL; vn = IDENTIFIER; ASSIGN; e = expression; SEMI
    { Ast.Local_variable_definition_statement(vn, Some e) }
  | LOCAL; vn = IDENTIFIER; SEMI
    { Ast.Local_variable_definition_statement(vn, None) }
  | vn = IDENTIFIER; ASSIGN; e = expression; SEMI
    { Ast.Assign_statement (vn, e) }
  | fn = IDENTIFIER; LPAREN; args = separated_list(COMMA, expression); RPAREN; SEMI
    { Ast.Function_call_statement (fn, args) }
  | RETURN; e = expression; SEMI
    { Ast.Return_statement (Some e) }
  | RETURN; SEMI
    { Ast.Return_statement (None) }
  ;
expression
  : id = IDENTIFIER
    { Ast.Variable_expression (id) }
  | n = NUMBER
    { Ast.Number_expression (n) }
  | TRUE
    { Ast.Boolean_expression (true) }
  | FALSE
    { Ast.Boolean_expression (false) }
  | fn = IDENTIFIER; LPAREN; args = separated_list(COMMA, expression); RPAREN
    { Ast.Function_call_expression (fn, args) }
  | lhs = expression; PLUS; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Plus, rhs) }
  | lhs = expression; MINUS; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Minus, rhs) }
  | lhs = expression; MULT; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Mult, rhs) }
  | lhs = expression; DIV; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Div, rhs) }
  | lhs = expression; LT; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Lt, rhs) }
  | lhs = expression; LE; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Le, rhs) }
  | lhs = expression; GT; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Gt, rhs) }
  | lhs = expression; GE; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Ge, rhs) }
  | lhs = expression; EQ; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Eq, rhs) }
  | lhs = expression; NE; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Ne, rhs) }
  | lhs = expression; AND; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.And, rhs) }
  | lhs = expression; OR; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Or, rhs) }        
  ;
