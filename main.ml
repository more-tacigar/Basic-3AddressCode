class generator head = object(self)
  val mutable current_number = 0
  method current =
    self#generate_variable_name current_number
  method generate =
    current_number <- current_number + 1;
    self#generate_variable_name current_number
  method private generate_variable_name number =
    head ^ (string_of_int number)
end
                                  
let translate program =
  let buffer = Buffer.create 100 in
  let generator = new generator "__tmp" in
  let label_generator = new generator "L" in

  let rec translate_external_definition = function
    | Ast.Variable_definition (id, exp) ->
       begin
         match exp with
         | Some x ->
            translate_expression (x);
            let current = generator#current in
            Buffer.add_string buffer id;
            Buffer.add_string buffer " = ";
            Buffer.add_string buffer current;
            Buffer.add_char buffer '\n'
         | None ->
            Buffer.add_string buffer id;
            Buffer.add_string buffer " = ";
            Buffer.add_string buffer "0\n"
       end
    | Ast.Function_definition (fn, params, stmts) ->
       Buffer.add_string buffer "func begin ";
       Buffer.add_string buffer fn;
       Buffer.add_char buffer '\n';
       List.iter translate_statement stmts;
       Buffer.add_string buffer "func end\n"
                         
  and translate_statement = function
    | Ast.If_statement (condition, tstmt, fstmt) ->
       translate_expression condition;
       let current = generator#current in
       Buffer.add_string buffer "if ";
       Buffer.add_string buffer current;
       Buffer.add_string buffer " goto ";
       let label = label_generator#generate in
       Buffer.add_string buffer label;
       Buffer.add_char buffer '\n';
       translate_statement tstmt;
       begin
         match fstmt with
         | Some x ->
            Buffer.add_string buffer label;
            Buffer.add_string buffer ": ";
            translate_statement x
         | None ->
            Buffer.add_string buffer label;
            Buffer.add_string buffer ": "
       end
    | Ast.For_statement (init_stmt, condition, prop_stmt, stmt) ->
       translate_statement init_stmt;
       let label = label_generator#generate in
       Buffer.add_string buffer label;
       Buffer.add_string buffer ": ";
       translate_expression condition;
       let current = generator#current in
       Buffer.add_string buffer "if ";
       Buffer.add_string buffer current;
       Buffer.add_string buffer " goto ";
       let label2 = label_generator#generate in
       Buffer.add_string buffer label2;
       Buffer.add_char buffer '\n';
       translate_statement prop_stmt;
       translate_statement stmt;
       Buffer.add_string buffer "goto ";
       Buffer.add_string buffer label;
       Buffer.add_char buffer '\n';
       Buffer.add_string buffer label2;
       Buffer.add_string buffer ": "
    | Ast.While_statement (condition, stmt) ->
       let label = label_generator#generate in
       Buffer.add_string buffer label;
       Buffer.add_string buffer ": ";
       translate_expression condition;
       let current = generator#current in
       Buffer.add_string buffer "if ";
       Buffer.add_string buffer current;
       Buffer.add_string buffer " goto ";
       let label2 = label_generator#generate in
       Buffer.add_string buffer label2;
       Buffer.add_char buffer '\n';
       translate_statement stmt;
       Buffer.add_string buffer "goto ";
       Buffer.add_string buffer label;
       Buffer.add_char buffer '\n';
       Buffer.add_string buffer label2;
       Buffer.add_string buffer ": "
    | Ast.Block_statement (stmts) ->
       List.iter translate_statement stmts
    | Ast.Local_variable_definition_statement (id, exp) ->
       begin
         match exp with
         | Some x ->
            translate_expression (x);
            let current = generator#current in
            Buffer.add_string buffer id;
            Buffer.add_string buffer " = ";
            Buffer.add_string buffer current;
            Buffer.add_char buffer '\n'
         | None ->
            Buffer.add_string buffer id;
            Buffer.add_string buffer " = ";
            Buffer.add_string buffer "0\n"
       end
    | Ast.Assign_statement (id, exp) ->
       translate_expression exp;
       let current = generator#current in
       Buffer.add_string buffer id;
       Buffer.add_string buffer " = ";
       Buffer.add_string buffer current;
       Buffer.add_char buffer '\n'
    | Ast.Function_call_statement (id, exps) ->
       let temps = List.map (fun exp ->
                       translate_expression exp;
                       generator#current) exps in
       List.iter (fun x ->
           Buffer.add_string buffer "param ";
           Buffer.add_string buffer x;
           Buffer.add_char buffer '\n') temps;
       Buffer.add_string buffer "call ";
       Buffer.add_string buffer id;
       Buffer.add_string buffer (string_of_int (List.length temps));
       Buffer.add_char buffer '\n'
                       
    | Ast.Return_statement (exp) ->
       match exp with
       | Some x ->
          translate_expression x;
          let current = generator#current in
          Buffer.add_string buffer "return ";
          Buffer.add_string buffer current;
          Buffer.add_char buffer '\n'
       | None ->
          Buffer.add_string buffer "return\n"
       
  and translate_expression = function
    | Ast.Variable_expression (id) ->
       let temp = generator#generate in
       Buffer.add_string buffer temp;
       Buffer.add_string buffer " = ";
       Buffer.add_string buffer id;
       Buffer.add_char buffer '\n'
    | Ast.Number_expression (n) ->
       let temp = generator#generate in
       Buffer.add_string buffer temp;
       Buffer.add_string buffer " = ";
       Buffer.add_string buffer (string_of_int n);
       Buffer.add_char buffer '\n'
    | Ast.Boolean_expression (b) ->
       let temp = generator#generate in
       Buffer.add_string buffer temp;
       Buffer.add_string buffer " = ";
       Buffer.add_string buffer (string_of_bool b);
       Buffer.add_char buffer '\n'
    | Ast.Function_call_expression (id, exps) ->
       let temps = List.map (fun exp ->
                       translate_expression exp;
                       generator#current) exps in
       List.iter (fun x ->
           Buffer.add_string buffer "param ";
           Buffer.add_string buffer x;
           Buffer.add_char buffer '\n') temps;
       let temp = generator#generate in
       Buffer.add_string buffer temp;
       Buffer.add_string buffer " = call ";
       Buffer.add_string buffer id;
       Buffer.add_string buffer (string_of_int (List.length temps));
       Buffer.add_char buffer '\n'
    | Ast.Binary_operation_expression (lhs, op, rhs) ->
       translate_expression lhs;
       let current1 = generator#current in
       translate_expression rhs;
       let current2 = generator#current in
       let temp = generator#generate in
       let opstr = translate_binary_operator op in
       Buffer.add_string buffer temp;
       Buffer.add_string buffer " = ";
       Buffer.add_string buffer current1;
       Buffer.add_string buffer opstr;
       Buffer.add_string buffer current2;
       Buffer.add_char buffer '\n'

  and translate_binary_operator = function
    | Ast.Plus  -> " + "
    | Ast.Minus -> " - "
    | Ast.Mult  -> " * "
    | Ast.Div   -> " / "
    | Ast.Lt    -> " < "
    | Ast.Le    -> " <= "
    | Ast.Gt    -> " > "
    | Ast.Ge    -> " >= "
    | Ast.Eq    -> " == "
    | Ast.Ne    -> " != "
    | Ast.And   -> " && "
    | Ast.Or    -> " || "
  in
  
  List.iter (fun external_definition ->
      translate_external_definition (external_definition)) program;
  buffer

let main () =
  let filename = Sys.argv.(1) in
  let lexbuf = Lexing.from_channel (open_in filename) in
  let program = Parser.program Lexer.read lexbuf in
  let result = translate program in
  print_string (Buffer.contents result)

let () = main ()
