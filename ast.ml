type identifier = string
 and program = external_definition list
 and external_definition =
   | Function_definition of identifier * identifier list * statement list
   | Variable_definition of identifier * expression option
 and statement =
   | If_statement of expression * statement * statement option
   | For_statement of statement * expression * statement * statement
   | While_statement of expression * statement
   | Block_statement of statement list
   | Local_variable_definition_statement of identifier * expression option
   | Assign_statement of identifier * expression
   | Function_call_statement of identifier * expression list
   | Return_statement of expression option
 and expression =
   | Variable_expression of identifier
   | Number_expression of int
   | Boolean_expression of bool 
   | Function_call_expression of identifier * expression list
   | Binary_operation_expression of expression * binary_operator * expression
 and binary_operator =
   | Plus | Minus | Mult | Div
   | Lt | Le | Gt | Ge | Ne | Eq | And | Or
                                           
