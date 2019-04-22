open CTypes
open Printf

let rec parseRet expr =
    match expr with
    | Constant(x) -> x
    | Neg(x) -> 65535 land (-(parseRet(x)))
    | BitComp(x) -> 65535 land (lnot (parseRet(x)))
    | Not(x) -> if parseRet x = 0 then 1 else 0
    | Add(x, y) -> 65535 land (parseRet x + parseRet y)
    | Sub(x, y) -> 65535 land (parseRet x - parseRet y)
    | Mul(x, y) -> 65535 land (parseRet x * parseRet y)
    | Div(x, y) -> 65535 land (parseRet x / parseRet y)



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

