type _type =
  | TypeInt
  | TypeVoid
  | TypeString
  | TypeBool
  | TypeClass of string
  | TypeArray of _type * int
[@@deriving show { with_path = false }]

type modifier = 
  | Public 
  | Static
  | Const
  | Async
[@@deriving show { with_path = false }]

type values = (* C_Type *)
  | ValInt of int
  | ValString of string (* string?? *)
  | ValNull
  | ValVoid
  | ValBool of bool
  (* ValClass? *)
[@@deriving show {with_path= false}]

and obj_ref =
  | ObjNull
  | ObjRef of string * string option (*class key and parent key??*)
[@@deriving show {with_path= false}]

type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | PreInc of expr
  | PostInc of expr
  | PreDec of expr
  | PostDec of expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Equal of expr * expr
  | NonEqual of expr * expr
  | Less of expr * expr
  | More of expr * expr
  | LessEqual of expr * expr
  | MoreEqual of expr * expr
  | Null
  | Var of string
  | ConstE of values
  | Assign of expr * expr (* = *)
  | FuncCall of string * expr list (* CallMethod *)
  | Await of expr
  | ArrayAccess of expr * expr
  | Lambda of expr (* not implemented *)
  | Linq of expr (* not implemented *)

and stmt =
  | Expr of expr (*???*)
  | StmtBlock of stmt list
  | For of stmt option * expr option * expr list * stmt (*because you can write for(int i = 0, j = 5; i < 4; i++, j--)*)
  | If of expr * stmt * stmt option (*statement option is "else"*)
  | While of expr * stmt
  | Return of expr option
  | Break
  | Continue
  | VarDeclr of modifier option * _type * (string * expr option) list
  | WriteLine of expr

and field = (* ??? *)
  | VarField of _type * (string * expr option) list
  | Method of _type * string * (_type * string) list * stmt
[@@deriving show { with_path = false }]

and c_sharp_class =
  | Class of modifier list * string * (modifier list * field) list(* Methods *)
[@@deriving show { with_path = false }]
