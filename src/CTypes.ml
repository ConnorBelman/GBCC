type data_type =
    | Int_Type

type expr =
    | ID of string
    | Constant of int

type stmt =
    | NoOp
    | Program of stmt
    | Function of expr * stmt
    | Return of expr

type value =
    | Int_Val of int

type environment = (string * value) list
