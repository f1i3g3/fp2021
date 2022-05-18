(* open Ast
open Parser
open Parser.Expr
open Parser.Stmt

let%test _ =
  apply_parser get_modifier_list "public static" = Some [Public; Static]

let%test _ = apply_parser convert_to_int "1" = Some 1
let%test _ = apply_parser null "  null" = Some Null
let%test _ = apply_parser ident_obj "   car" = Some "car"
let%test _ = apply_parser ident_obj "  Cat1" = Some "Cat1"
let%test _ = apply_parser get_variable "   Cat" = Some (Var "Cat")

(* let%test _ =
  apply_parser parse_string "\"Parse\"" = Some (ConstE (ValString "Parse")) *)

let%test _ = apply_parser atomic "      123123" = Some (ConstE (ValInt 123123))
let%test _ = apply_parser ident_obj "  123Ret" = None

(* let%test _ =
  apply_parser atomic "\"Parse\"" = Some (ConstE (ValString "Parse")) *)

let%test _ = apply_parser atomic "   true" = Some (ConstE (ValBool true))
let%test _ = apply_parser atomic "   false" = Some (ConstE (ValBool false))
let%test _ = apply_parser atomic "   null" = Some Null
let%test _ = apply_parser define_type "  int" = Some TypeInt
let%test _ = apply_parser define_type "  void" = Some TypeVoid
let%test _ = apply_parser define_type "  string" = Some TypeString

let%test _ =
  apply_parser expr "a = b = 1"
  = Some (Assign (Var "a", Assign (Var "b", ConstE (ValInt 1))))

let%test _ =
  apply_parser expr "4 + 5"
  = Some (Add (ConstE (ValInt 4), ConstE (ValInt 5)))

let%test _ =
  apply_parser expr "2/5 + 3 * (5 % 3)"
  = Some
      (Add
         ( Div (ConstE (ValInt 2), ConstE (ValInt 5))
         , Mul (ConstE (ValInt 3), Mod (ConstE (ValInt 5), ConstE (ValInt 3)))
         ))

let%test _ =
  apply_parser expr "x = true"
  = Some (Assign (Var "x", ConstE (ValBool true)))

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
             , [ ConstE (ValInt 5); Var "arg2"
               ; Mul (Var "arg3", ConstE (ValInt 3)) ] ) ))
 *)
(* let%test _ =
  apply_parser expr "Sum(obj.a, 3)"
  = Some
      (FuncCall
         ("Sum", [Access (Var "obj", Var "a"); ConstE (ValInt 3)])) *)
(* 
let%test _ =
  apply_parser expr "new Shop(5,\"MVideo\")"
  = Some
      (ClassCreate ("Shop", [ConstE (ValInt 5); ConstE (ValString "MVideo")])) *)

(* let%test _ =
  apply_parser expr "Fork(new Child())"
  = Some (FuncCall ("Fork", [ClassCreate ("Child", [])])) *)

(* -----------------------  STATEMENTS ------------------------*)

let%test _ =
  apply_parser parse_statement "int a = 0, b = 1, c = 2;"
  = Some
      (VarDeclr
         ( None
         , TypeInt
         , [ ("a", Some (ConstE (ValInt 0))); ("b", Some (ConstE (ValInt 1)))
           ; ("c", Some (ConstE (ValInt 2))) ] ))

let%test _ =
  apply_parser parse_statement "const int a = 0, b = 1, c = 2;"
  = Some
      (VarDeclr
         ( Some Const
         , TypeInt
         , [ ("a", Some (ConstE (ValInt 0))); ("b", Some (ConstE (ValInt 1)))
           ; ("c", Some (ConstE (ValInt 2))) ] ))

let%test _ = apply_parser break "break;" = Some Break
let%test _ = apply_parser continue "continue;" = Some Continue

let%test _ =
  apply_parser return_stat "return 3;"
  = Some (Return (Some (ConstE (ValInt 3))))

let%test _ =
  apply_parser parse_statement {|  if(num1 > num2)
{
}   |}
  = Some (If (More (Var "num1", Var "num2"), Block [], None))

(* let%test _ =
  apply_parser parse_statement
    {|  if(num1 > num2)
{        Console.WriteLine("help");
}   |}
  = Some
      (If
         ( More (Var "num1", Var "num2")
         , Block [Print (ConstE (ValString "help"))]
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
         , Block [Print (Access (Var "a", FuncCall ("Sum", [])))]
         , Some
             (If
                ( Less (Var "num1", Var "num2")
                , Block [Print (Access (Var "a", Var "b"))]
                , Some (Block [Print (ConstE (ValString "2"))]) )) )) *)
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
         ( More (Var "i", ConstE (ValInt 0))
         , Block
             [Print (Var "i"); Expression (PostDec (Var "i"))] )) *)

(* let%test _ =
  apply_parser parse_statement
    {| for (int i = 0; i < 9; i++)
{
    Console.WriteLine(1);
}|}
  = Some
      (For
         ( Some (VarDeclr (None, TypeInt, [("i", Some (ConstE (ValInt 0)))]))
         , Some (Less (Var "i", ConstE (ValInt 9)))
         , [PostInc (Var "i")]
         , Block [Print (ConstE (ValInt 1))] )) *)

(* let%test _ =
  apply_parser parse_statement
    {| { if(a<b) {}
for (; ;)
{
    
} }
|}
  = Some
      (Block
         [ If (Less (Var "a", Var "b"), Block [], None)
         ; For (None, None, [], Block [Print (ConstE (ValString "1"))])
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
         , Some (Less (Var "i", ConstE (ValInt 9)))
         , []
         , Block [Print (Mul (Var "i", Var "i"))] ))

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
         ( Some (VarDeclr (None, TypeInt, [("i", Some (ConstE (ValInt 0)))]))
         , Some (Less (Var "i", ConstE (ValInt 9)))
         , [PostInc (Var "i")]
         , Block
             [ If (Equal (Var "i", ConstE (ValInt 5)), Break, None)
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
          , Block
              [ VarDeclr (None, TypeInt, [("hour", Some (ConstE (ValInt 23)))])
              ; If
                  ( More (Var "hour", ConstE (ValInt 22))
                  , Block [Return None]
                  , Some (Block [Print (ConstE (ValString "Hello"))])
                  ) ] ) )

let%test _ =
  apply_parser class_elements
    {| public Person(string n) { name = n; age = 18; }|}
  = Some
      ( [Public]
      , Constructor
          ( "Person"
          , [(String, "n")]
          , Block
              [ Expression (Assign (Var "name", Var "n"))
              ; Expression (Assign (Var "age", ConstE (ValInt 18))) ] ) )
 *)               *)  