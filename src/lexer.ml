open Str
open TokenTypes

let reg_space = Str.regexp "[ \n\t]+"
let reg_ID = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let reg_int = Str.regexp "[0-9]+"

(* matches a symbol with it's token and the strings length *)
let reg_operators : (Str.regexp * (token * int)) list =
    List.map (fun (s, t, l) -> ((Str.regexp s), (t, l)))
        [
        ("(",                       Tok_LParen,         1);
        (")",                       Tok_RParen,         1);
        ("{",                       Tok_LBrace,         1);
        ("}",                       Tok_RBrace,         1);
        (";",                       Tok_Semi,           1);
        ("int[ \n\t]+",             Tok_Int_Type,       3);
        ("return[ \n\t]+",          Tok_Return,         6);
        ("if\\([ \n\t]+\\|(\\)",    Tok_If,             2);
        ("else\\([ \n\t]+\\|{\\)",  Tok_Else,           4);
        ("+=",                      Tok_PlusEqual,      2);
        ("-=",                      Tok_MinEqual,       2);
        ("/=",                      Tok_DivEqual,       2);
        ("*=",                      Tok_MulEqual,       2);
        ("%=",                      Tok_ModEqual,       2);
        ("<<=",                     Tok_LShiftEqual,    3);
        (">>=",                     Tok_RShiftEqual,    3);
        ("&=",                      Tok_AndEqual,       2);
        ("|=",                      Tok_OrEqual,        2);
        ("\\^=",                    Tok_XorEqual,       2);
        ("-",                       Tok_Neg,            1);
        ("~",                       Tok_BitComp,        1);
        ("+",                       Tok_Plus,           1);
        ("*",                       Tok_Mul,            1);
        ("/",                       Tok_Div,            1);
        ("&&",                      Tok_And,            2);
        ("||",                      Tok_Or,             2);
        ("<<",                      Tok_ShiftLeft,      2);
        (">>",                      Tok_ShiftRight,     2);
        ("==",                      Tok_Equal,          2);
        ("!=",                      Tok_NotEqual,       2);
        ("<=",                      Tok_LessEqual,      2);
        (">=",                      Tok_GreaterEqual,   2);
        ("<",                       Tok_Less,           1);
        (">",                       Tok_Greater,        1);
        ("!",                       Tok_Not,            1);
        ("&",                       Tok_BitAnd,         1);
        ("|",                       Tok_BitOr,          1);
        ("\\^",                     Tok_BitXor,         1);
        ("=",                       Tok_Assign,         1);
        ("%",                       Tok_Mod,            1);
        (":",                       Tok_Colon,          1);
        ("?",                       Tok_Question,       1)
        ]
;;

(* iterates through list of regexps, and returns token/length pair if one matches *)
let rec op_match s pos ops =
    match ops with
        | [] -> None
        | (re, (tk,l))::ops' ->
            if (Str.string_match re s pos) then Some (tk,l)
            else op_match s pos ops'

let tokenize str =
    let rec tok pos s acc =
        if pos >= String.length s then
            (*List.rev @@ EOF::*)acc
        else if (Str.string_match reg_space s pos) then
            tok (pos + 1) s acc
        else if (Str.string_match reg_ID s pos) then
            let id_token = Str.matched_string s in
            match (op_match s pos reg_operators) with
                | Some(tk, len) -> tok (pos + len) s (tk::acc)
                | None -> tok (pos + (String.length id_token)) s ((Tok_ID id_token)::acc)
        else if (Str.string_match reg_int s pos) then
            let int_token = Str.matched_string s in
            tok (pos + (String.length int_token)) s ((Tok_Int (int_of_string int_token))::acc)
        else
            match (op_match s pos reg_operators) with
                | Some(tk, len) -> tok (pos + len) s (tk::acc)
                | None -> raise (InvalidInputException "tokenize") in
    tok 0 str []
