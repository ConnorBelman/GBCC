open CTypes
open Printf

let rec parseInt expr =
    match expr with
    | Constant(x) -> x
    | Neg(x) -> 65535 land (-(parseInt(x)))
    | BitComp(x) -> 65535 land (lnot (parseInt(x)))
    | Not(x) -> if parseInt x = 0 then 1 else 0


let rec code_gen ast file =
    match ast with
    | Program(s) -> code_gen s file
    | Function(e, s) ->
        (match e with
        | ID(name) ->
            fprintf file "_%s:\n\t" name;
            code_gen s file
        | _ -> printf "fail")
    | Return(e) -> fprintf file "ld de,$%04X\n\tret\n" (parseInt e)

