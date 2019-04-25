open CTypes
open Printf

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

let rec parseRet expr file =
    match expr with
    | Constant(x) -> fprintf file "\tld a,$%02X\n" x
    (* | Constant(x) -> x *)
    | Neg(x) ->
        parseRet x file;
        fprintf file "\tcpl\n\tinc a\n"
    (* | Neg(x) -> 255 land (-(parseRet x)) *)
    | BitComp(x) ->
        parseRet x file;
        fprintf file "\tcpl\n"
    (* | BitComp(x) -> 255 land (lnot (parseRet x)) *)
    | BitOr(x, y) ->
        parseRet y file;
        fprintf file "\tpush af\n";
        parseRet x file;
        fprintf file "\tpop bc\n\tor b\n"
    (* | BitOr(x, y) -> 255 land (parseRet x lor parseRet y) *)
    | BitXor(x, y) ->
        parseRet y file;
        fprintf file "\tpush af\n";
        parseRet x file;
        fprintf file "\tpop bc\n\txor b\n"
    (* | BitXor(x, y) -> 255 land (parseRet x lxor parseRet y) *)
    | BitAnd(x, y) ->
        parseRet y file;
        fprintf file "\tpush af\n";
        parseRet x file;
        fprintf file "\tpop bc\n\tand b\n"
    (* | BitAnd(x, y) -> 255 land (parseRet x land parseRet y) *)
    (* | ShiftLeft(x, y) -> 255 land (parseRet x lsl parseRet y) *)
    (* | ShiftRight(x, y) -> 255 land (parseRet x lsr parseRet y) *)
    | Add(x, y) ->
        parseRet y file;
        fprintf file "\tpush af\n";
        parseRet x file;
        fprintf file "\tpop bc\n\tadd a,b\n"
    (* | Add(x, y) -> 255 land (parseRet x + parseRet y) *)
    | Sub(x, y) ->
        parseRet y file;
        fprintf file "\tpush af\n";
        parseRet x file;
        fprintf file "\tpop bc\n\tsub b\n"
    (* | Sub(x, y) -> 255 land (parseRet x - parseRet y) *)
    (* | Mul(x, y) -> 255 land (parseRet x * parseRet y) *)
    (* | Div(x, y) -> 255 land (parseRet x / parseRet y) *)
    | Not(x) ->
        let j = fresh() in
        parseRet x file;
        fprintf file "\tand a\n\tld a,$00\n\tjp nz,_cp%d\n\tld a,$01\n_cp%d:\n" j j
    (* | Not(x) -> if parseRet x = 0 then 1 else 0 *)
    | And(x, y) ->
        let j = fresh() in
        parseRet x file;
        fprintf file "\tand a\n\tjp z,_cp%d\n" j;
        parseRet y file;
        fprintf file "\tand a\n\tjp z,_cp%d\n\tld a,$01\n_cp%d:\n" j j
    (* | And(x, y) -> if (parseRet x != 0) && (parseRet y != 0) then 1 else 0 *)
    | Or(x, y) ->
        let j1 = fresh() in
        let j2 = fresh() in
        parseRet x file;
        fprintf file "\tand a\n\tjp z,_cp%d\n\tld a,$01\n\tjp _cp%d\n_cp%d:\n" j1 j2 j1;
        parseRet y file;
        fprintf file "\tand a\n\tjp z,_cp%d\n\tld a,$01\n_cp%d:\n" j2 j2
    (* | Or(x, y) -> if (parseRet x != 0) || (parseRet y != 0) then 1 else 0 *)
    | Equal(x, y) ->
        let j = fresh() in
        parseRet y file;
        fprintf file "\tpush af\n\t";
        parseRet x file;
        fprintf file "\tpop bc\n\tcp b\n\tld a,$00\n\tjp nz,_cp%d\n\tld a,$01\n_cp%d:\n" j j
    (* | Equal(x, y) -> if parseRet x = parseRet y then 1 else 0 *)
    | NotEqual(x, y) ->
        parseRet y file;
        fprintf file "\tpush af\n";
        parseRet x file;
        fprintf file "\tpop bc\n\tsub b\n"
    (* | NotEqual(x, y) -> if parseRet x != parseRet y then 1 else 0 *)
    | Less(x, y) ->
        let j = fresh() in
        parseRet y file;
        fprintf file "\tpush af\n";
        parseRet x file;
        fprintf file "\tpop bc\n\tcp b\n\tld a,$00\n\tjp nc,_cp%d\n\tld a,$01\n_cp%d:\n" j j
    (* | Less(x, y) -> if parseRet x < parseRet y then 1 else 0 *)
    | Greater(x, y) ->
        let j = fresh() in
        parseRet x file;
        fprintf file "\tpush af\n";
        parseRet y file;
        fprintf file "\tpop bc\n\tcp b\n\tld a,$00\n\tjp nc,_cp%d\n\tld a,$01\n_cp%d:\n" j j
    (* | Greater(x, y) -> if parseRet x > parseRet y then 1 else 0 *)
    | LessEqual(x, y) ->
        let j = fresh() in
        parseRet x file;
        fprintf file "\tpush af\n";
        parseRet y file;
        fprintf file "\tpop bc\n\tcp b\n\tld a,$00\n\tjp c,_cp%d\n\tld a,$01\n_cp%d:\n" j j
    (* | LessEqual(x, y) -> if parseRet x <= parseRet y then 1 else 0 *)
    | GreaterEqual(x, y) ->
        let j = fresh() in
        parseRet y file;
        fprintf file "\tpush af\n";
        parseRet x file;
        fprintf file "\tpop bc\n\tcp b\n\tld a,$00\n\tjp c,_cp%d\n\tld a,$01\n_cp%d:\n" j j
    (* | GreaterEqual(x, y) -> if parseRet x >= parseRet y then 1 else 0 *)



let rec code_gen ast file =
    match ast with
    | Program(s) -> code_gen s file
    | Function(x, s) ->
        fprintf file "_%s:\n" x;
        code_gen s file
    | Return(e) ->
        parseRet e file;
        fprintf file "\tld l,a\n\tpush hl\n\tpop de\n\tret\n"
    (* | Return(e) -> fprintf file "ld de,$%04X\n\tret\n" (parseRet e file) *)

