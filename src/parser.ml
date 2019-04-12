open Utils
open TokenTypes
open CTypes

type stmt_result = token list * stmt
type expr_result = token list * expr

(* consumes a token if it matches the expected token *)
let match_token (toks : token list) (tok : token) : token list =
    match toks with
        | [] -> raise (InvalidInputException(string_of_token tok))
        | h::t when h = tok -> t
        | h::_ -> raise (InvalidInputException(
            Printf.sprintf "Expected %s from input %s, got %s"
            (string_of_token tok)
            (string_of_list string_of_token toks)
            (string_of_token h)))

let rec match_tokens (toks : token list) (t_lst : token list) : token list =
    match t_lst with
        | [] -> toks
        | h::t -> match_tokens (match_token toks h) t

(* returns the next token in the token list *)
let lookahead toks =
    match toks with
        | [] -> raise (InvalidInputException "no tokens")
        | h::t -> h

let rec parse_Expr toks =
    match lookahead toks with
        | Tok_ID(x) -> (match_token toks (lookahead toks), ID(x))
        | Tok_Int(x) -> (match_token toks (lookahead toks), Constant(x))
        | _ -> raise (InvalidInputException "statement token sent parse_Expr")

let rec parse_Program toks =
    let (t,s) = parse_Function toks in (t, Program(s))
and parse_Function toks =
    let t = match_token toks Tok_Int_Type in
    let (t', id) = parse_Expr t in
    let t'' = match_tokens t' [Tok_LParen; Tok_RParen; Tok_LBrace] in
    let (t3, s) = parse_Statement t'' in
    let t4 = match_token t3 Tok_RBrace in (t4, Function(id, s))
and parse_Statement toks =
    let t = match_token toks Tok_Return in
    let (t', e) = parse_Expr t in
    let t'' = match_token t' Tok_Semi in (t'', Return(e))

let parse_main toks =
    let (t, s) = parse_Program toks in
    let t' = match_token t EOF in s

