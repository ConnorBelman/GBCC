open CTypes
open Printf
open Utils

let rec var_contains lst var =
    match lst with
    | [] -> false
    | (id, offset)::t -> if id = var then true else var_contains t var

let rec var_find lst var =
    match lst with
    | [] -> raise (Failure(Printf.sprintf "Error: variable %s not initialized" var))
    | (id, offset)::t -> if id = var then offset else var_find t var

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

let rec code_gen_expr e env file =
    match e with
    | ID(x) ->
        let var_offset = var_find env x in
        fprintf file "\tld h,b\n\tld l,c\n\tld de,#0x%04X\n\tadd hl,de\n\tld a,(hl)\n" (65535 land var_offset)
    | Assign(x, e) ->
        code_gen_expr e env file;
        let var_offset = var_find env x in
        fprintf file "\tld h,b\n\tld l,c\n\tld de,#0x%04X\n\tadd hl,de\n\tld (hl),a\n" (65535 land var_offset)
    | Constant(x) -> fprintf file "\tld a,#0x%02X\n" x
    (* | Constant(x) -> x *)
    | Neg(x) ->
        code_gen_expr x env file;
        fprintf file "\tcpl\n\tinc a\n"
    (* | Neg(x) -> 255 land (-(parseRet x)) *)
    | BitComp(x) ->
        code_gen_expr x env file;
        fprintf file "\tcpl\n"
    (* | BitComp(x) -> 255 land (lnot (parseRet x)) *)
    | BitOr(x, y) ->
        code_gen_expr y env file;
        fprintf file "\tpush af\n";
        code_gen_expr x env file;
        fprintf file "\tpop de\n\tor d\n"
    (* | BitOr(x, y) -> 255 land (parseRet x lor parseRet y) *)
    | BitXor(x, y) ->
        code_gen_expr y env file;
        fprintf file "\tpush af\n";
        code_gen_expr x env file;
        fprintf file "\tpop de\n\txor d\n"
    (* | BitXor(x, y) -> 255 land (parseRet x lxor parseRet y) *)
    | BitAnd(x, y) ->
        code_gen_expr y env file;
        fprintf file "\tpush af\n";
        code_gen_expr x env file;
        fprintf file "\tpop de\n\tand d\n"
    (* | BitAnd(x, y) -> 255 land (parseRet x land parseRet y) *)
    | ShiftLeft(x, y) ->
        let j = fresh() in
        code_gen_expr x env file;
        fprintf file "\tpush af\n";
        code_gen_expr y env file;
        fprintf file "\tpop de\n_sl%d:\n\tand a\n\tjp z,_slend%d\n\tsla d\n\tdec a\n\tjp _sl%d\n_slend%d:\n\tld a,d\n" j j j j
    (* | ShiftLeft(x, y) -> 255 land (parseRet x lsl parseRet y) *)
    | ShiftRight(x, y) ->
        let j = fresh() in
        code_gen_expr x env file;
        fprintf file "\tpush af\n";
        code_gen_expr y env file;
        fprintf file "\tpop de\n_sr%d:\n\tand a\n\tjp z,_srend%d\n\tsrl d\n\tdec a\n\tjp _sr%d\n_srend%d:\n\tld a,d\n" j j j j
    (* | ShiftRight(x, y) -> 255 land (parseRet x lsr parseRet y) *)
    | Add(x, y) ->
        code_gen_expr y env file;
        fprintf file "\tpush af\n";
        code_gen_expr x env file;
        fprintf file "\tpop de\n\tadd a,d\n"
    (* | Add(x, y) -> 255 land (parseRet x + parseRet y) *)
    | Sub(x, y) ->
        code_gen_expr y env file;
        fprintf file "\tpush af\n";
        code_gen_expr x env file;
        fprintf file "\tpop de\n\tsub d\n"
    (* | Sub(x, y) -> 255 land (parseRet x - parseRet y) *)
    | Mul(x, y) ->
        let j = fresh() in
        code_gen_expr x env file;
        fprintf file "\tpush af\n";
        code_gen_expr y env file;
        fprintf file "\tand a\n\tjp z,_multz%d\n\tld d,a\n\tdec d\n\tpop af\n\tand a\n\tjp z,_multz%d\n\tld e,a\n_mult%d:\n\tadd a,e\n\tdec d\n\tjp nz,_mult%d\n\tjp _multend%d\n_multz%d:\n\tld a,#0x00\n_multend%d:\n" j j j j j j j
    (* | Mul(x, y) -> 255 land (parseRet x * parseRet y) *)
    | Div(x, y) ->
        let j = fresh() in
        code_gen_expr y env file;
        fprintf file "\tpush af\n";
        code_gen_expr x env file;
        fprintf file "\tpop de\n\tld e,#0x00\n_div%d:\n\tcp d\n\tjp c,_divend%d\n\tinc e\n\tsub d\n\tjp _div%d\n_divend%d:\n\tld a,e\n" j j j j
    (* | Div(x, y) -> 255 land (parseRet x / parseRet y) *)
    | Mod(x, y) ->
        let j = fresh() in
        code_gen_expr y env file;
        fprintf file "\tpush af\n";
        code_gen_expr x env file;
        fprintf file "\tpop de\n_mod%d:\n\tcp d\n\tjp c,_modend%d\n\tsub d\n\tjp _mod%d\n_modend%d:\n " j j j j
    (* | Mod(x, y) -> 255 land (parseRet x % parseRet y) *)
    | Not(x) ->
        let j = fresh() in
        code_gen_expr x env file;
        fprintf file "\tand a\n\tld a,#0x00\n\tjp nz,_not%d\n\tld a,#0x01\n\tand a\n_not%d:\n" j j
    (* | Not(x) -> if parseRet x = 0 then 1 else 0 *)
    | And(x, y) ->
        let j = fresh() in
        code_gen_expr x env file;
        fprintf file "\tand a\n\tjp z,_and%d\n" j;
        code_gen_expr y env file;
        fprintf file "\tand a\n\tjp z,_and%d\n\tld a,#0x01\n_and%d:\n" j j
    (* | And(x, y) -> if (parseRet x != 0) && (parseRet y != 0) then 1 else 0 *)
    | Or(x, y) ->
        let j = fresh() in
        code_gen_expr x env file;
        fprintf file "\tand a\n\tjp z,_or%d\n\tld a,#0x01\n\tjp _orend%d\n_or%d:\n" j j j;
        code_gen_expr y env file;
        fprintf file "\tand a\n\tjp z,_orend%d\n\tld a,#0x01\n_orend%d:\n" j j
    (* | Or(x, y) -> if (parseRet x != 0) || (parseRet y != 0) then 1 else 0 *)
    | Equal(x, y) ->
        let j = fresh() in
        code_gen_expr y env file;
        fprintf file "\tpush af\n";
        code_gen_expr x env file;
        fprintf file "\tpop de\n\tcp d\n\tld a,#0x00\n\tjp nz,_eq%d\n\tld a,#0x01\n_eq%d:\n" j j
    (* | Equal(x, y) -> if parseRet x = parseRet y then 1 else 0 *)
    | NotEqual(x, y) ->
        let j = fresh() in
        code_gen_expr y env file;
        fprintf file "\tpush af\n";
        code_gen_expr x env file;
        fprintf file "\tpop de\n\tcp d\n\tld a,#0x00\n\tjp z,_neq%d\n\tld a,#0x01\n_neq%d:\n" j j
    (* | NotEqual(x, y) -> if parseRet x != parseRet y then 1 else 0 *)
    | Less(x, y) ->
        let j = fresh() in
        code_gen_expr y env file;
        fprintf file "\tpush af\n";
        code_gen_expr x env file;
        fprintf file "\tpop de\n\tcp d\n\tld a,#0x00\n\tjp nc,_ls%d\n\tld a,#0x01\n_ls%d:\n" j j
    (* | Less(x, y) -> if parseRet x < parseRet y then 1 else 0 *)
    | Greater(x, y) ->
        let j = fresh() in
        code_gen_expr x env file;
        fprintf file "\tpush af\n";
        code_gen_expr y env file;
        fprintf file "\tpop de\n\tcp d\n\tld a,#0x00\n\tjp nc,_gr%d\n\tld a,#0x01\n_gr%d:\n" j j
    (* | Greater(x, y) -> if parseRet x > parseRet y then 1 else 0 *)
    | LessEqual(x, y) ->
        let j = fresh() in
        code_gen_expr x env file;
        fprintf file "\tpush af\n";
        code_gen_expr y env file;
        fprintf file "\tpop de\n\tcp d\n\tld a,#0x00\n\tjp c,_leq%d\n\tld a,#0x01\n_leq%d:\n" j j
    (* | LessEqual(x, y) -> if parseRet x <= parseRet y then 1 else 0 *)
    | GreaterEqual(x, y) ->
        let j = fresh() in
        code_gen_expr y env file;
        fprintf file "\tpush af\n";
        code_gen_expr x env file;
        fprintf file "\tpop de\n\tcp d\n\tld a,#0x00\n\tjp c,_geq%d\n\tld a,#0x01\n_geq%d:\n" j j
    (* | GreaterEqual(x, y) -> if parseRet x >= parseRet y then 1 else 0 *)
    | Ternary(x, y, z) ->
        let j = fresh() in
        code_gen_expr x env file;
        fprintf file "\tand a\n\tjp z,_cond%d\n" j;
        code_gen_expr y env file;
        fprintf file "\tjp _condend%d\n_cond%d:\n" j j;
        code_gen_expr z env file;
        fprintf file "_condend%d:\n" j
    (*| _ -> printf "Error: unmatched expession in parseRet" *)

let rec code_gen_statement s env scope_env si file =
    match s with
    | Return(e) ->
        code_gen_expr e env file;
        fprintf file "\tld e,a\n\tld h,b\n\tld l,c\n\tld sp,hl\n\tpop bc\n\tret\n";
    | Expr(e) ->
        code_gen_expr e env file;
    | Conditional(e, s, o) ->
        let j = fresh() in
        code_gen_expr e env file;
        (match o with
        | Some s' ->
            fprintf file "\tand a\n\tjp z,_else%d\n" j;
            code_gen_statement s env scope_env si file;
            fprintf file "\tjp _ifend%d\n_else%d:\n" j j;
            code_gen_statement s' env scope_env si file;
            fprintf file "_ifend%d:\n" j
        | None ->
            fprintf file "\tand a\n\tjp z,_ifend%d\n" j;
            code_gen_statement s env scope_env si file;
            fprintf file "_ifend%d:\n" j)
    | Compound(b) -> code_gen_block b env [] si file
    | _ -> printf "Error: unmatched statement in code_gen_statement"
and code_gen_declaration d t env scope_env si file =
    match d with
    | Declare(x, e_opt) ->
        if var_contains scope_env x then printf "Error: var %s already initialized" x else
        (match e_opt with
        | None -> fprintf file "\tld l,#0x00\n\tpush hl\n"
        | Some (e) ->
            code_gen_expr e env file;
            fprintf file "\tld l,a\n\tpush hl\n";);
        code_gen_block t ((x, si)::env) ((x, si)::scope_env) (si - 2) file
and code_gen_block lst env scope_env si file =
    match lst with
    | [] -> fprintf file "\tadd sp,#0x%04X\n" (2 * List.length scope_env)
    | Statement(s)::t ->
        code_gen_statement s env scope_env si file;
        code_gen_block t env scope_env si file
    | Declaration(d)::t -> code_gen_declaration d t env scope_env si file

let rec code_gen_program ast file =
    match ast with
    | Program(s) -> code_gen_program s file
    | Function(x, s) ->
        fprintf file "_%s:\n\tpush bc\n\tlda hl,00(sp)\n\tld b,h\n\tld c,l\n" x;
        code_gen_block s [] [] (-2) file
    | _ -> printf "Error: unmatched statement in code_gen_program"
    (* | Return(e) -> fprintf file "ld de,$%04X\n\tret\n" (parseRet e file) *)