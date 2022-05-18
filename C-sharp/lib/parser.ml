open Ast
open Opal

let apply_parser parser input = parse parser (LazyStream.of_string input)

let reserved =
  [ "true"; "false"; "if"; "else"; "while"; "public"; "static"; "const"
  ; "void"; "string"; "char" ; "int"; "bool"; "for"; "null"; "new"
  ; "return"; "break"; "continue"; "class" ; "async" ; "await" ; "select" ; "from" ]

let const = token "const" >> return Const
let parens = between (token "(") (token ")") 
let braces = between (token "{") (token "}")
let ar_braces = between (token "[") (token "]")

let digits = spaces >> many1 digit => implode
let convert_to_int = digits => int_of_string

let get_modifier_list =
  many
    (choice
       [ token "public" >> return Public; token "static" >> return Static
       ; token "const" >> return Const; token "async" >> return Async ] )

module Expr = struct
  open Ast

  let parse_string = (* char array *)
    let string_of_chars chars = (* to string *)
      let buf = Buffer.create 16 in (* 20 *)
      List.iter (Buffer.add_char buf) chars ;
      Buffer.contents buf in
    token "\""
    >> many (satisfy (fun x -> x <> '\"'))
    >>= fun list ->
    token "\"" >> return (ConstE (ValString (string_of_chars list)))

  let add_op = token "+" >> return (fun x y -> Add (x, y))
  let sub_op = token "-" >> return (fun x y -> Sub (x, y))
  let mul_op = token "*" >> return (fun x y -> Mul (x, y))
  let div_op = token "/" >> return (fun x y -> Div (x, y))
  let mod_op = token "%" >> return (fun x y -> Mod (x, y))
  let or_op = token "||" >> return (fun x y -> Or (x, y))
  let and_op = token "&&" >> return (fun x y -> And (x, y))
  let l_op = token "<" >> return (fun x y -> Less (x, y))
  let m_op = token ">" >> return (fun x y -> More (x, y))
  let le_op = token "<=" >> return (fun x y -> LessEqual (x, y))
  let me_op = token ">=" >> return (fun x y -> MoreEqual (x, y))
  let eq_op = token "==" >> return (fun x y -> Equal (x, y))
  let neq_op = token "!=" >> return (fun x y -> NonEqual (x, y))
  let null = token "null" >> return Null

  let ident_obj =
    spaces >> letter <~> many alpha_num => implode
    >>= function x when List.mem x reserved -> mzero | x -> return x

  let get_var = ident_obj => fun x -> Var x

  let get_arr_var = ident_obj >>= fun x -> 
    ar_braces(convert_to_int) => fun n -> ArrayAccess (Var x, ConstE (ValInt n))

  let atomic =
    get_arr_var
    <|> get_var
    <|> (convert_to_int >>= fun n -> return (ConstE (ValInt n)))
    <|> parse_string
    <|> (token "false" >> return (ConstE (ValBool false))) (* unite with basic functions/parse_string?? *)
    <|> (token "true" >> return (ConstE (ValBool true)))
    <|> null

  let def_type =
    choice
      [ token "int" >> return TypeInt; token "string" >> return TypeString
      ; token "void" >> return TypeVoid; token "Bool" >> return TypeBool
      ; (ident_obj >>= fun class_id -> return (TypeClass class_id)) ]

  let rec expr input = num_expr input

  and num_expr input = (chainl1 and_expr or_op) input

  and and_expr input = (chainl1 comp_expr and_op) input

  and comp_expr input =
    (chainl1 add_expr (le_op <|> me_op <|> l_op <|> m_op <|> eq_op <|> neq_op))
      input

  and add_expr input = (chainl1 mul_expr (add_op <|> sub_op)) input
  and mul_expr input = (chainl1 unar_expr (mul_op <|> div_op <|> mod_op)) input

  and unar_expr input =
    choice
      [ (token "!" >> lexeme primar_expr >>= fun x -> return (Not x))
      ; (token "await" >> lexeme primar_expr >>= fun x -> return (Await x))
      ; ( token "-" >> lexeme primar_expr
        >>= fun x -> return (Sub (ConstE (ValInt 0), x)) )
      ; (token "++" >> lexeme primar_expr >>= fun x -> return (PreInc x))
      ; (token "--" >> lexeme primar_expr >>= fun x -> return (PreDec x))
      ; (lexeme primar_expr >>= fun x -> token "++" >> return (PostInc x))
      ; (lexeme primar_expr >>= fun x -> token "--" >> return (PostDec x))
      ; primar_expr ]
      input

  and primar_expr input =
    (assign <|> func_call <|> parens expr <|> atomic)
      input

  and sep_by_comma input = sep_by expr (token ",") input

  and func_call input =
    ( ident_obj
    >>= fun name ->
    token "(" >> sep_by_comma
    >>= fun args_list -> token ")" >> return (FuncCall (name, args_list)) )
      input

  and assign input =
    let parse_left = func_call <|> get_arr_var <|> get_var in
    ( parse_left
    >>= fun left ->
    token "=" >> expr >>= fun right -> return (Assign (left, right)) )
      input

end

module Stmt = struct
  open Expr

  let rec parse_statement input =
    choice
      [ continue; break; parse_expr; return_stmt; if_stmt
      ; while_stmt ; var_declr; for_stmt; stmt_block; writeline ]
      input

  and if_stmt input =
    ( token "if" >> parens expr
    >>= fun condition ->
    parse_statement
    >>= fun then_stmt ->
    choice
      [ ( token "else" >> parse_statement
        >>= fun else_stmt -> return (If (condition, then_stmt, Some else_stmt))
        ); return (If (condition, then_stmt, None)) ] )
      input

  and while_stmt input =
    ( token "while" >> parens expr
    >>= fun condition ->
    parse_statement >>= fun stmt -> return (While (condition, stmt)) )
      input

  and var_declr input =
    let helper =
      ident_obj
      >>= fun var_name ->
      token "=" >> expr
      >>= (fun var_value -> return (var_name, Some var_value))
      <|> return (var_name, None) in
    choice
      [ ( const
        >>= fun const ->
        def_type
        >>= fun var_type ->
        sep_by1 helper (token ",")
        >>= fun var_pair ->
        token ";" >> return (VarDeclr (Some const, var_type, var_pair)) )
      ; ( def_type
        >>= fun var_type ->
        sep_by1 helper (token ",")
        >>= fun var_pair ->
        token ";" >> return (VarDeclr (None, var_type, var_pair)) ) 
      ; ( def_type (* array var decl with dimension = 0 *)
        >>= fun var_type ->
        ident_obj >>= fun one_var -> 
        token "[" >> token "]" >> 
        token ";" >> return (VarDeclr (None, TypeArray(var_type, 0), [(one_var, None)])) )
      ; ( def_type (* array var decl with dimension <> 0 *)
        >>= fun var_type ->
        ident_obj >>= fun one_var ->
        ar_braces (convert_to_int) >>= fun dim -> 
        token ";" >> return (VarDeclr (None, TypeArray(var_type, dim), [(one_var, None)])) )
        ]
      input

  and stmt_block input =
    ( braces (sep_by parse_statement spaces)
    >>= fun stmts -> return (StmtBlock stmts) )
      input

  and for_stmt input =
    ( token "for" >> token "("
    >> choice
         [ (var_declr >>= fun stmt -> return (Some stmt))
         ; token ";" >> return None ]
    >>= fun declare ->
    choice
      [ (expr >>= fun expr -> token ";" >> return (Some expr))
      ; token ";" >> return None ]
    >>= fun condition ->
    sep_by expr (token ",")
    >>= fun after ->
    token ")" >> parse_statement
    >>= fun body -> return (For (declare, condition, after, body)) )
      input

  and return_stmt input =
    ( token "return"
    >> choice
         [ ( skip_many1 space >> expr
           >>= fun result -> token ";" >> return (Return (Some result)) )
         ; token ";" >> return (Return None) ] )
      input

  and parse_expr input =
    (expr >>= fun exprs -> token ";" >> return (Expr exprs)) input
  
  and continue input = (token "continue" >> token ";" >> return Continue) input
   
  and break input = (token "break" >> token ";" >> return Break) input

  and writeline input =
    ( token "Console.WriteLine(" >> expr
    >>= fun wl_expr -> token ");" >> return (WriteLine wl_expr) )
      input

end

let get_params =
  Expr.def_type
  >>= fun _type -> Expr.ident_obj >>= fun name -> return (_type, name)

let field =
  let helper =
    Expr.ident_obj
    >>= fun name ->
    token "=" >> Expr.expr
    >>= (fun value -> return (name, Some value))
    <|> return (name, None) in
  Expr.def_type
  >>= fun f_type ->
  sep_by helper (token ",")
  >>= fun var_list -> token ";" >> return (VarField (f_type, var_list))

let class_method =
  Expr.def_type
  >>= fun method_type ->
  Expr.ident_obj
  >>= fun method_name ->
  token "("
  >> sep_by get_params (token ",")
  >>= fun params_list ->
  token ")" >> Stmt.stmt_block
  >>= fun stmt_block ->
  return (Method (method_type, method_name, params_list, stmt_block))

let class_elements =
  get_modifier_list
  >>= fun modifiers ->
  field <|> class_method
  >>= fun class_elem -> return (modifiers, class_elem)

let parse_class =
  get_modifier_list
  >>= fun modifiers ->
  token "class" >> Expr.ident_obj
  >>= fun name ->
  choice
    [ (token ":" >> Expr.ident_obj >>= fun parent -> return (Some parent))
    ; return None ]
  >>= fun _parent ->
  token "{"
  >> sep_by class_elements spaces
  >>= fun class_elements ->
  token "}" >> return (Class (modifiers, name, class_elements))

let parser = parse_class
