open CTypes
open Printf

let rec parseRet expr file =
    match expr with
    | Constant(x) -> fprintf file "ld a,$%02X\n\t" x
    (* | Neg(x) ->255 land (-(parseRet x))
    | BitComp(x) -> 255 land (lnot (parseRet x))
    | BitOr(x, y) -> 255 land (parseRet x lor parseRet y)
    | BitXor(x, y) -> 255 land (parseRet x lxor parseRet y)
    | BitAnd(x, y) -> 255 land (parseRet x land parseRet y)
    | ShiftLeft(x, y) -> 255 land (parseRet x lsl parseRet y)
    | ShiftRight(x, y) -> 255 land (parseRet x lsr parseRet y) *)
    | Add(x, y) ->
        parseRet x file;
        fprintf file "push af\n\t";
        parseRet y file;
        fprintf file "pop bc\n\tadd a,b\n\t"

    (* | Sub(x, y) -> 255 land (parseRet x - parseRet y)
    | Mul(x, y) -> 255 land (parseRet x * parseRet y)
    | Div(x, y) -> 255 land (parseRet x / parseRet y)
    | Not(x) -> if parseRet x = 0 then 1 else 0
    | And(x, y) -> if (parseRet x != 0) && (parseRet y != 0) then 1 else 0
    | Or(x, y) -> if (parseRet x != 0) || (parseRet y != 0) then 1 else 0
    | Equal(x, y) -> if parseRet x = parseRet y then 1 else 0
    | NotEqual(x, y) -> if parseRet x != parseRet y then 1 else 0
    | Less(x, y) -> if parseRet x < parseRet y then 1 else 0
    | Greater(x, y) -> if parseRet x > parseRet y then 1 else 0
    | LessEqual(x, y) -> if parseRet x <= parseRet y then 1 else 0
    | GreaterEqual(x, y) -> if parseRet x >= parseRet y then 1 else 0 *)



let rec code_gen ast file =
    match ast with
    | Program(s) -> code_gen s file
    | Function(x, s) ->
        fprintf file "_%s:\n\t" x;
        code_gen s file
    | Return(e) ->
        parseRet e file;
        fprintf file "ld l,a\n\tpush hl\n\tpop de\n\tret\n"
    (* | Return(e) -> fprintf file "ld de,$%04X\n\tret\n" (parseRet e file) *)

