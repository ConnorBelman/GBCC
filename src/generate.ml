open CTypes
open Printf

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

let rec parseRet expr file =
    match expr with
    | Constant(x) -> fprintf file "ld a,$%02X\n\t" x
    (* | Constant(x) -> x *)
    | Neg(x) ->
        parseRet x file;
        fprintf file "cpl\n\tinc a\n\t"
    (* | Neg(x) -> 255 land (-(parseRet x)) *)
    | BitComp(x) ->
        parseRet x file;
        fprintf file "cpl\n\t"
    (* | BitComp(x) -> 255 land (lnot (parseRet x)) *)
    | BitOr(x, y) ->
        parseRet y file;
        fprintf file "push af\n\t";
        parseRet x file;
        fprintf file "pop bc\n\tor b\n\t"
    (* | BitOr(x, y) -> 255 land (parseRet x lor parseRet y) *)
    | BitXor(x, y) ->
        parseRet y file;
        fprintf file "push af\n\t";
        parseRet x file;
        fprintf file "pop bc\n\txor b\n\t"
    (* | BitXor(x, y) -> 255 land (parseRet x lxor parseRet y) *)
    | BitAnd(x, y) ->
        parseRet y file;
        fprintf file "push af\n\t";
        parseRet x file;
        fprintf file "pop bc\n\tand b\n\t"
    (* | BitAnd(x, y) -> 255 land (parseRet x land parseRet y) *)
    (* | ShiftLeft(x, y) -> 255 land (parseRet x lsl parseRet y) *)
    (* | ShiftRight(x, y) -> 255 land (parseRet x lsr parseRet y) *)
    | Add(x, y) ->
        parseRet y file;
        fprintf file "push af\n\t";
        parseRet x file;
        fprintf file "pop bc\n\tadd a,b\n\t"
    (* | Add(x, y) -> 255 land (parseRet x + parseRet y) *)
    | Sub(x, y) ->
        parseRet y file;
        fprintf file "push af\n\t";
        parseRet x file;
        fprintf file "pop bc\n\tsub b\n\t"
    (* | Sub(x, y) -> 255 land (parseRet x - parseRet y)
    | Mul(x, y) -> 255 land (parseRet x * parseRet y)
    | Div(x, y) -> 255 land (parseRet x / parseRet y) *)
    (* | Not(x) -> if parseRet x = 0 then 1 else 0
    | And(x, y) -> if (parseRet x != 0) && (parseRet y != 0) then 1 else 0
    | Or(x, y) -> if (parseRet x != 0) || (parseRet y != 0) then 1 else 0 *)
    | Equal(x, y) ->
        let j = fresh() in
        parseRet y file;
        fprintf file "push af\n\t";
        parseRet x file;
        fprintf file "pop bc\n\tcp b\n\tld a,$00\n\tjp nz,_cp%d\n\tld a,$01\n_cp%d\n\tret\n\t" j j
    (* | Equal(x, y) -> if parseRet x = parseRet y then 1 else 0 *)
    | NotEqual(x, y) ->
        parseRet y file;
        fprintf file "push af\n\t";
        parseRet x file;
        fprintf file "pop bc\n\tsub b\n\t"
    (* | NotEqual(x, y) -> if parseRet x != parseRet y then 1 else 0 *)
    (* | Less(x, y) -> if parseRet x < parseRet y then 1 else 0
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

