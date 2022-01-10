type _type = (* CT_Type *)
  | TypeInt
  | TypeChar (* ?? *)
  | TypeVoid
  | TypeString
  | TypeClass of string
[@@deriving show { with_path = false }]
 (* Add more!!! *)

type modifier = (* ?? *)
  | Public 
  | Static
  | Const (* ?? *)
[@@deriving show { with_path = false }]

type values = (* C_Type *)
  | ValInt of int
  | ValChar of char
  | ValNull
  | ValArray of expr list (* ?? *)
[@@deriving show {with_path= false}]

and expr =
  | Value of values
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
  | FuncCall of string * expr list
[@@deriving show { with_path = false }]

and stmt = 
  | VarDeclr of modifier option * _type * (string * expr option) list (* ?? *)
  | Assign of expr * expr (* = *)
  | If of expr * stmt * stmt option (*if else*)
  | While of expr * stmt
  | For of stmt option * expr option * expr list * stmt
  | Break
  | Continue
  | Return of expr option
  | Block of stmt list
(* fun decl? *)
(* linq? *)
[@@deriving show { with_path = false }]

and field =
  | VarField of _type * (string * expr option) list
  | Method of _type * string * (_type * string) list * stmt
  | Constr of string * (_type * string) list * stmt
[@@deriving show { with_path = false }]

and c_sharp_class =
  | Class of modifier list * string * (modifier list * field) option (* Methods *)
[@@deriving show { with_path = false }]