open Ast
open Angstrom

(* Reserved words *)

let reserved_keywords =
  [ "if"; "else"; "while"; "for"; "return"; "break"; "continue"; "class" ]

let reserved_datatypes =
  [ "int"; "char"; "void" ]

(** Basic checkers **)

let is_reserved c = List.mem c reserved_keywords || List.mem c reserved_datatypes

let is_whitespace = function ' ' | '\t' -> true | _ -> false

let is_end_of_line = function '\n' -> true | _ -> false (* '\r' -> true *)

let is_digit = function '0' .. '9' -> true | _ -> false

let is_valid_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true (* First symbol?? *)
  | _ -> false

let is_valid_first_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false

(** Basic parser functions **)

let is_gap c = is_whitespace c || is_end_of_line c

let del_gap = take_while is_gap

let del_space = take_while is_whitespace

let del_space1 = take_while1 is_whitespace (* Do I need it? *)

let token s = del_space *> string s

let number =
  del_space *> take_while1 is_digit >>= fun n -> return @@ ValInt (int_of_string n)

let char_value =
  char '\'' *> any_char <* char '\'' >>= fun ch -> return @@ ValChar ch

let take_numbers_or_chars = take_while is_valid_char

(** Other basic parsers **)

let left_of p1 p = p <* del_space <* p1

let right_of p1 p = p1 *> del_space *> p

let between p1 p2 p = left_of p2 (right_of p1 p)

let parens p = between (token "(") (token ")") p

let braces p = between (token "{") (token "}") p

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init

let rec chainr1 e op =
  e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let lexeme p = p @@ skip_many del_gap


(* Expression parsing functions *)

module Expr = struct
  open Ast

  let f_add = token "+" *> return (fun e1 e2 -> Add (e1, e2))

  let f_sub = token "-" *> return (fun e1 e2 -> Sub (e1, e2))

  let f_mul = token "*" *> return (fun e1 e2 -> Mul (e1, e2))

  let f_div = token "/" *> return (fun e1 e2 -> Div (e1, e2))

  let f_mod = token "%" *> return (fun e1 e2 -> Mod (e1, e2))

  let f_not = token "!" *> return (fun e1 -> Not e1)

  let f_and = token "&&" *> return (fun e1 e2 -> And (e1, e2))

  let f_or = token "||" *> return (fun e1 e2 -> Or (e1, e2))

  let f_eq = token "==" *> return (fun e1 e2 -> Equal (e1, e2))

  let f_neq = token "!=" *> return (fun e1 e2 -> NonEqual (e1, e2))

  let f_ls = token "<" *> return (fun e1 e2 -> Less (e1, e2))

  let f_gr = token ">" *> return (fun e1 e2 -> More (e1, e2))

  let f_lseq = token "<=" *> return (fun e1 e2 -> LessEqual (e1, e2))

  let f_greq = token ">=" *> return (fun e1 e2 -> MoreEqual (e1, e2))

  let null = token "null" *> return Null

  let ident =
  del_space *> peek_char >>= function
    | Some c when is_valid_first_char c -> take_numbers_or_chars
      >>= fun id -> if is_reserved id then fail "Error: Found a reserved word!" else return id
    | _ -> fail "Error: Invalid identifier!"

  let def_type =
     choice
      [ token "int" *> return TypeInt; token "string" *> return TypeString; token "void" *> return TypeVoid;
      (ident >>= fun c_name -> return @@ TypeClass c_name) ] (* ??? *)

  let rec expr str = num_expr str

  and num_expr str = (chainl1 and_expr f_or) str

  and and_expr str = (chainl1 comp_expr f_and) str

  and comp_expr str =
    (chainl1 add_expr (f_lseq <|> f_greq <|> f_ls <|> f_gr <|> f_eq <|> f_neq)) str

  and add_expr str = (chainl1 mul_expr (f_add <|> f_sub)) str

  and mul_expr str = (chainl1 unar_expr (f_mul <|> f_div <|> f_mod)) str

  and unar_expr str =
    choice
      [ (token "!" *> lexeme primar_expr >>= fun x -> return (Not x))
      ; ( token "-" *> lexeme primar_expr
        >>= fun x -> return @@ Sub (ConstExpr (ValInt 0), x) )
      ; (token "++" *> lexeme primar_expr >>= fun x -> return (PreInc x))
      ; (lexeme primar_expr >>= fun x -> token "++" *> return (PostInc x))
      ; (token "--" *> lexeme primar_expr >>= fun x -> return (PreDec x))
      ; (lexeme primar_expr >>= fun x -> token "--" *> return (PostDec x))
      ; primar_expr ]
      str

  and primar_expr str =
    ( init_instance <|> assign <|> field_access <|> call_method <|> parens expr <|> atomic ) str

  and sep_comma str = sep_by expr (token ",") str

  and call_method str =
    ( ident
    >>= fun name ->
    token "(" *> sep_comma
    >>= fun args -> token ")" *> return (CallMethod (name, args)) )
      str

  and init_instance input =
    ( token "new" *> ident
    >>= fun name ->
    token "(" *> sep_comma
    >>= fun args_list -> token ")" *> return (ClassCreate (name, args_list)) )
      input

  and field_access input =
    let helper = parens init_instance <|> call_method <|> get_var in
    ( helper
    >>= fun head ->
    many1 (token "." *> helper)
    => fun tl -> List.fold_left (fun head tl -> Access (head, tl)) head tl )
      input

  and assign input =
    let parse_left = field_access <|> call_method <|> get_var in
    ( parse_left
    >>= fun left ->
    token "=" *> expr >>= fun right -> return (Assign (left, right)) )
      input

  (* arrays? *)

end


(* TODO: Adapt expression parser + rewrite AST *)

let get_modifiers =
  many 
    (choice 
      [ token "public" *> return Public ;  (*  token "public" >>= fun pub -> return @@ Public pub *)
      token "static" *> return Static ])

(* In-methods statements parsing *)

let block stmts =
  del_gap *> token "{"
  *> many1 (del_gap *> stmts <* del_gap)
  <* token "}"
  >>= fun return_block -> del_gap *> (return @@ Block return_block)

module Stmt = struct
    open Expr

    let rec parse_stmts str =
      choice
        [ parse_expr str; parse_if str; parse_for str; parse_while str; parse_break str; parse_continue str; parse_block str ]
    
    and parse_if str =
    ( token "if" *> parens expr 
      >>= fun if_stmt -> parse_stmts
      >>= fun then_stmt -> 
        choice
        [ ( token "else" *> parse_stmts
        >>= fun else_stmt -> return @@ If (if_stmt, then_stmt, else_stmt) );
        return @@ If (if_stmt, then_stmt, None) ] ) str

    and parse_for str = 
     ( token "for" *> token "("
      *> choice
         [ (var_declare >>= fun stmt -> return (Some stmt));
         token ";" *> return None ]
      >>= fun var_decl ->
      choice
        [ (expr >>= fun expr -> token ";" *> return (Some expr));
        token ";" *> return None ]
      >>= fun cond_stmt ->
        sep_by (token ",") expr
      >>= fun interrupt_stmt ->
        oken ")" *> parse_stmts
      >>= fun block -> return @@ For (var_decl, cond_stmt, interrupt_stmt, block) ) str

    and parse_while str = 
      ( token "while" *> parens expr
      >>= fun while_stmt -> parse_stmts
      >>= fun do_stmt -> return @@ While (while_stmt, do_stmt) ) str

    and parse_break str = (token "break" *> token ";" *> return Break) str

    and parse_continue str = (token "continue" *> token ";" *> return Continue) str

    and parse_block str =
      ( braces (sep_by spaces parse_stmts)
        >>= fun stmts -> return @@ Block stmts ) str

    and return_stat str =
    ( token "return" *> choice
         [ ( del_gap *> expr
           >>= fun res -> token ";" *> return @@ Return (Some result) );
         token ";" *> return @@ Return None ] )
      str

    and parse_expr str =
      (expr >>= fun _expr -> token ";" *> return @@ Expression _expr) str
    
    and parse_var str =
      let helper = ident
        >>= fun name ->
        token "=" *> expr
        >>= (fun value -> return (name, Some value))
        <|> return (name, None)
      in choice
        [ ( const
          >>= fun modifier ->
          def_type
          >>= fun v_type ->
          sep_by1 helper (token ",")
          >>= fun r_val ->
          token ";" *> return (VarDeclr (Some modifier, v_type, r_val)) );
          ( def_type
          >>= fun v_type ->
          sep_by1 helper (token ",")
          >>= fun r_val ->
          token ";" *> return (VarDeclr (None, v_type, r_val)) ) ]
      str


 end

(** Class parser function **)

let get_params =
  Expr.def_type
  >>= fun p_type -> Expr.ident
  >>= fun p_name -> return (p_type, p_name) 

let parse_field =
  del_gap *> let one_var =
    Expr.ident
      >>= fun name ->
        token "=" *> Expr.expr
        >>= fun value -> return (name, Some value)
        <|> return (name, None)
      in
    Expr.def_type
    >>= fun f_type ->
      sep_by (token ",") one_var
      >>= fun vars -> token ";" *> return @@ VarField (f_type, vars)


let parse_method =
  Expr.def_type
  >>= fun m_type ->
    Expr.ident
    >>= fun name ->
      token "(" *> sep_by (token ",") get_params
      >>= fun m_params ->
        token ")" *> Stmt.parse_block
        >>= fun block -> 
          return @@ Method (m_type, name, m_params, block)

let parse_constr =
  Expr.ident
  >>= fun c_name ->
    token "(" *> sep_by (token ",") get_params
    >>= fun constr_params ->
      token ")" *> Stmt.parse_block
      >>= fun block -> 
        return @@ Method (c_name, constr_params, block)

let parse_elements =
  del_gap *> get_modifier_list >>= 
  fun modifiers ->
    parse_field <|> parse_method <|> parse_constr
    >>= fun elements -> 
      return (modifiers, elements)

let parse_class = (* async *)
  del_gap *> get_modifiers >== (* del_gap?? *)
  fun modifiers -> 
    token "class" *> Expr.ident (* name + arguments *)
    >== fun name ->
      token "{" 
      *> sep_by del_gap parse_elements 
      >== fun elements ->
        token "}" *> return @@ Class (modifiers, name, elements) <* del_gap

(** Main parser function **)

let apply parser str = parse_string ~consume:All parser str

