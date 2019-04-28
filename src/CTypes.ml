type data_type =
    | Int_Type

type expr =
    | ID of string
    | Constant of int
    | Neg of expr
    | BitComp of expr
    | BitOr of expr * expr
    | BitAnd of expr * expr
    | BitXor of expr * expr
    | ShiftLeft of expr * expr
    | ShiftRight of expr * expr
    | Not of expr
    | Add of expr * expr
    | Sub of expr * expr
    | Mul of expr * expr
    | Div of expr * expr
    | Mod of expr * expr
    | And of expr * expr
    | Or of expr * expr
    | Equal of expr * expr
    | NotEqual of expr * expr
    | Greater of expr * expr
    | GreaterEqual of expr * expr
    | Less of expr * expr
    | LessEqual of expr * expr

type stmt =
    | NoOp
    | Program of stmt
    | Function of string * stmt list
    | Return of expr
    | Declare of string * expr option
    | Expr of expr
    | Assign of string * expr

type value =
    | Int_Val of int

type environment = (string * value) list
