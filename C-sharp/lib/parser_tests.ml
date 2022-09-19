open Ast
open Parser
open Parser.Expr
open Parser.Stmt

let%test _ =
  apply_parser get_modifiers "public static" = Some [Public; Static]

let%test _ = apply_parser convert_to_int "1" = Some 1
let%test _ = apply_parser null "  null" = Some Null
let%test _ = apply_parser ident_obj "   car" = Some "car"
let%test _ = apply_parser ident_obj "  Cat1" = Some "Cat1"
let%test _ = apply_parser get_variable "   Cat" = Some (Var "Cat")

(* let%test _ =
  apply_parser parse_string "\"Parse\"" = Some (ConstExpr (ValString "Parse")) *)

let%test _ = apply_parser atomic "      123123" = Some (ConstExpr (ValInt 123123))
let%test _ = apply_parser ident_obj "  123Ret" = None

(* let%test _ =
  apply_parser atomic "\"Parse\"" = Some (ConstExpr (ValString "Parse")) *)

let%test _ = apply_parser atomic "   true" = Some (ConstExpr (ValBool true))
let%test _ = apply_parser atomic "   false" = Some (ConstExpr (ValBool false))
let%test _ = apply_parser atomic "   null" = Some Null
let%test _ = apply_parser def_type "  int" = Some TypeInt
let%test _ = apply_parser def_type "  void" = Some TypeVoid
let%test _ = apply_parser def_type "  string" = Some String

let%test _ =
  apply_parser expr "a = b = 1"
  = Some (Assign (Var "a", Assign (Var "b", ConstExpr (ValInt 1))))

let%test _ =
  apply_parser expr "4 + 5"
  = Some (Add (ConstExpr (ValInt 4), ConstExpr (ValInt 5)))

let%test _ =
  apply_parser expr "2/5 + 3 * (5 % 3)"
  = Some
      (Add
         ( Div (ConstExpr (ValInt 2), ConstExpr (ValInt 5))
         , Mul (ConstExpr (ValInt 3), Mod (ConstExpr (ValInt 5), ConstExpr (ValInt 3)))
         ))

let%test _ =
  apply_parser expr "x = true"
  = Some (Assign (Var "x", ConstExpr (ValBool true)))

(* let%test _ =
  apply_parser expr "a.b.c"
  = Some (Access (Access (Var "a", Var "b"), Var "c")) *)

(* let%test _ =
  apply_parser expr "obj.Sum(5, arg2, arg3 * 3)"
  = Some
      (Access
         ( Var "obj"
         , FuncCall
             ( "Sum"
             , [ ConstExpr (ValInt 5); Var "arg2"
               ; Mul (Var "arg3", ConstExpr (ValInt 3)) ] ) ))
 *)
(* let%test _ =
  apply_parser expr "Sum(obj.a, 3)"
  = Some
      (FuncCall
         ("Sum", [Access (Var "obj", Var "a"); ConstExpr (ValInt 3)])) *)
(* 
let%test _ =
  apply_parser expr "new Shop(5,\"MVideo\")"
  = Some
      (ClassCreate ("Shop", [ConstExpr (ValInt 5); ConstExpr (ValString "MVideo")])) *)

(* let%test _ =
  apply_parser expr "Fork(new Child())"
  = Some (FuncCall ("Fork", [ClassCreate ("Child", [])])) *)

(* -----------------------  STATEMENTS ------------------------*)

let%test _ =
  apply_parser parse_stmts "int a = 0, b = 1, c = 2;"
  = Some
      (VarDeclr
         ( None
         , TypeInt
         , [ ("a", Some (ConstExpr (ValInt 0))); ("b", Some (ConstExpr (ValInt 1)))
           ; ("c", Some (ConstExpr (ValInt 2))) ] ))

let%test _ =
  apply_parser parse_stmts "const int a = 0, b = 1, c = 2;"
  = Some
      (VarDeclr
         ( Some Const
         , TypeInt
         , [ ("a", Some (ConstExpr (ValInt 0))); ("b", Some (ConstExpr (ValInt 1)))
           ; ("c", Some (ConstExpr (ValInt 2))) ] ))

let%test _ = apply_parser parse_break "break;" = Some Break
let%test _ = apply_parser parse_continue "continue;" = Some Continue

let%test _ =
  apply_parser parse_return "return 3;"
  = Some (Return (Some (ConstExpr (ValInt 3))))

let%test _ =
  apply_parser parse_stmts {|  if(num1 > num2)
{
}   |}
  = Some (If (More (Var "num1", Var "num2"), StmtsBlock [], None))

(* let%test _ =
  apply_parser parse_statement
    {|  if(num1 > num2)
{        Console.WriteLine("help");
}   |}
  = Some
      (If
         ( More (Var "num1", Var "num2")
         , StmtsBlock [Print (ConstExpr (ValString "help"))]
         , None )) *)

(* let%test _ =
  apply_parser parse_statement
    {|  if(num1 > num2)
{
    Console.WriteLine(a.Sum());
}
else if (num1 < num2)
{
    Console.WriteLine(a.b);
}
else
{
    Console.WriteLine("2");
} |}
  = Some
      (If
         ( More (Var "num1", Var "num2")
         , StmtsBlock [Print (Access (Var "a", FuncCall ("Sum", [])))]
         , Some
             (If
                ( Less (Var "num1", Var "num2")
                , StmtsBlock [Print (Access (Var "a", Var "b"))]
                , Some (StmtsBlock [Print (ConstExpr (ValString "2"))]) )) )) *)
(* 
let%test _ =
  apply_parser parse_statement
    {| while (i > 0)
{
    Console.WriteLine(i);
    i--;
} |}
  = Some
      (While
         ( More (Var "i", ConstExpr (ValInt 0))
         , StmtsBlock
             [Print (Var "i"); Expression (PostDec (Var "i"))] )) *)

(* let%test _ =
  apply_parser parse_statement
    {| for (int i = 0; i < 9; i++)
{
    Console.WriteLine(1);
}|}
  = Some
      (For
         ( Some (VarDeclr (None, TypeInt, [("i", Some (ConstExpr (ValInt 0)))]))
         , Some (Less (Var "i", ConstExpr (ValInt 9)))
         , [PostInc (Var "i")]
         , StmtsBlock [Print (ConstExpr (ValInt 1))] )) *)

(* let%test _ =
  apply_parser parse_statement
    {| { if(a<b) {}
for (; ;)
{
    
} }
|}
  = Some
      (StmtsBlock
         [ If (Less (Var "a", Var "b"), StmtsBlock [], None)
         ; For (None, None, [], StmtsBlock [Print (ConstExpr (ValString "1"))])
         ])

let%test _ =
  apply_parser parse_statement
    {|  for (; i<9;)
{
    Console.WriteLine(i*i);
} |}
  = Some
      (For
         ( None
         , Some (Less (Var "i", ConstExpr (ValInt 9)))
         , []
         , StmtsBlock [Print (Mul (Var "i", Var "i"))] ))

let%test _ =
  apply_parser parse_statement
    {|  for (int i = 0; i < 9; i++)
{
    if (i == 5)
        break;
    Console.WriteLine(i);
}|}
  = Some
      (For
         ( Some (VarDeclr (None, TypeInt, [("i", Some (ConstExpr (ValInt 0)))]))
         , Some (Less (Var "i", ConstExpr (ValInt 9)))
         , [PostInc (Var "i")]
         , StmtsBlock
             [ If (Equal (Var "i", ConstExpr (ValInt 5)), Break, None)
             ; Print (Var "i") ] )) *)



(* let%test _ =
  apply_parser class_elements {|  public int sum;|}
  = Some ([Public], VariableField (TypeInt, [("sum", None)])) *)

(* let%test _ =
  apply_parser class_elements
    {| static void SayHello()
{
    int hour = 23;
    if(hour > 22)
    {
        return;
    }
    else
    {
        Console.WriteLine("Hello");
    }
}
|}
  = Some
      ( [Static]
      , Method
          ( Void
          , "SayHello"
          , []
          , StmtsBlock
              [ VarDeclr (None, TypeInt, [("hour", Some (ConstExpr (ValInt 23)))])
              ; If
                  ( More (Var "hour", ConstExpr (ValInt 22))
                  , StmtsBlock [Return None]
                  , Some (StmtsBlock [Print (ConstExpr (ValString "Hello"))])
                  ) ] ) )

let%test _ =
  apply_parser class_elements
    {| public Person(string n) { name = n; age = 18; }|}
  = Some
      ( [Public]
      , Constructor
          ( "Person"
          , [(String, "n")]
          , StmtsBlock
              [ Expression (Assign (Var "name", Var "n"))
              ; Expression (Assign (Var "age", ConstExpr (ValInt 18))) ] ) )
 *)
  