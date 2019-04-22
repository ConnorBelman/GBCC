open CTypes
open Printf

let rec parseRet expr =
    match expr with
    | Constant(x) -> x
    | Neg(x) -> 65535 land (-(parseRet x))
    | BitComp(x) -> 65535 land (lnot (parseRet x))
    | BitOr(x, y) -> 65535 land (parseRet x lor parseRet y)
    | BitXor(x, y) -> 65535 land (parseRet x lxor parseRet y)
    | BitAnd(x, y) -> 65535 land (parseRet x land parseRet y)
    | ShiftLeft(x, y) -> 65535 land (parseRet x lsl parseRet y)
    | ShiftRight(x, y) -> 65535 land (parseRet x lsr parseRet y)
    | Add(x, y) -> 65535 land (parseRet x + parseRet y)
    | Sub(x, y) -> 65535 land (parseRet x - parseRet y)
    | Mul(x, y) -> 65535 land (parseRet x * parseRet y)
    | Div(x, y) -> 65535 land (parseRet x / parseRet y)
    | Not(x) -> if parseRet x = 0 then 1 else 0
    | And(x, y) -> if (parseRet x != 0) && (parseRet y != 0) then 1 else 0
    | Or(x, y) -> if (parseRet x != 0) || (parseRet y != 0) then 1 else 0
    | Equal(x, y) -> if parseRet x = parseRet y then 1 else 0
    | NotEqual(x, y) -> if parseRet x != parseRet y then 1 else 0
    | Less(x, y) -> if parseRet x < parseRet y then 1 else 0
    | Greater(x, y) -> if parseRet x > parseRet y then 1 else 0
    | LessEqual(x, y) -> if parseRet x <= parseRet y then 1 else 0
    | GreaterEqual(x, y) -> if parseRet x >= parseRet y then 1 else 0



let rec code_gen ast file =
    match ast with
    | Program(s) -> code_gen s file
    | Function(e, s) ->
        (match e with
        | ID(name) ->
            fprintf file "_%s:\n\t" name;
            code_gen s file
        | _ -> printf "fail")
    | Return(e) -> fprintf file "ld de,$%04X\n\tret\n" (parseRet e)

