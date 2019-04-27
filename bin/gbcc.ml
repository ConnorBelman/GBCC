open Printf
open TokenTypes
open Lexer
open Parser
open Generate

let c_file = Sys.argv.(2)
let s_file = Sys.argv.(1)

let toks = ref [] ;;

let () =
    let ic = open_in c_file in
    let oc = open_out s_file in
    try
        while true; do
            let line = input_line ic in
            toks := (tokenize line) @ !toks;
        done;
            close_in ic;
    with e ->
        close_in_noerr ic;
    let tokens = List.rev @@ EOF::!toks in
    let ast = parse_main tokens in
    code_gen_program ast oc
