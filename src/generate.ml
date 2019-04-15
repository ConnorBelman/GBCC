open CTypes
open Printf

let rec code_gen ast file =
    match ast with
    | Program(s) -> code_gen s file
    | Function(e, s) ->
        (match e with
        | ID(name) ->
            fprintf file "_%s:\n\t" name;
            code_gen s file
        | _ -> printf "fail")
    | Return(e) ->
        (match e with
        | Constant(x) -> fprintf file "ld de,$%04x\n" x
        | _ -> printf "fail")


