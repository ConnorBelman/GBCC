type data_type =
    | Int_Type

type expr =
    | ID of string
    | Constant of int
    | Neg of expr
    | BitComp of expr
    | Not of expr
    | Add of expr * expr
    | Sub of expr * expr
    | Mul of expr * expr
    | Div of expr * expr

type stmt =
    | NoOp
    | Program of stmt
    | Function of expr * stmt
    | Return of expr

type value =
    | Int_Val of int

type environment = (string * value) list
