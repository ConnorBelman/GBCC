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

(* consumes a list of tokens if they match the expected tokens *)
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
    | Tok_ID(x) ->
        let t = match_token toks (Tok_ID(x)) in
        (match lookahead t with
        | Tok_Assign ->
            let t' = match_token t Tok_Assign in
            let (t'', e) = parse_Expr t' in
            (t'', Assign(x, e))
        | Tok_PlusEqual ->
            let t' = match_token t Tok_PlusEqual in
            let (t'', e) = parse_Expr t' in
            (t'', Assign(x, Add(ID(x), e)))
        | Tok_MinEqual ->
            let t' = match_token t Tok_MinEqual in
            let (t'', e) = parse_Expr t' in
            (t'', Assign(x, Sub(ID(x), e)))
        | Tok_DivEqual ->
            let t' = match_token t Tok_DivEqual in
            let (t'', e) = parse_Expr t' in
            (t'', Assign(x, Div(ID(x), e)))
        | Tok_MulEqual ->
            let t' = match_token t Tok_MulEqual in
            let (t'', e) = parse_Expr t' in
            (t'', Assign(x, Mul(ID(x), e)))
        | Tok_ModEqual ->
            let t' = match_token t Tok_ModEqual in
            let (t'', e) = parse_Expr t' in
            (t'', Assign(x, Mod(ID(x), e)))
        | Tok_LShiftEqual ->
            let t' = match_token t Tok_LShiftEqual in
            let (t'', e) = parse_Expr t' in
            (t'', Assign(x, ShiftLeft(ID(x), e)))
        | Tok_RShiftEqual ->
            let t' = match_token t Tok_RShiftEqual in
            let (t'', e) = parse_Expr t' in
            (t'', Assign(x, ShiftRight(ID(x), e)))
        | Tok_AndEqual ->
            let t' = match_token t Tok_AndEqual in
            let (t'', e) = parse_Expr t' in
            (t'', Assign(x, BitAnd(ID(x), e)))
        | Tok_OrEqual ->
            let t' = match_token t Tok_OrEqual in
            let (t'', e) = parse_Expr t' in
            (t'', Assign(x, BitOr(ID(x), e)))
        | Tok_XorEqual ->
            let t' = match_token t Tok_XorEqual in
            let (t'', e) = parse_Expr t' in
            (t'', Assign(x, BitXor(ID(x), e)))
        | Tok_Semi -> (t, ID(x))
        | _ -> parse_ConditionalExpr toks)
    | _ -> parse_ConditionalExpr toks
and parse_ConditionalExpr toks =
    let (t, e) = parse_OrExpr toks in
    match lookahead t with
    | Tok_Question ->
        let t' = match_token t Tok_Question in
        let (t'', e') = parse_Expr t' in
        let t3 = match_token t'' Tok_Colon in
        let (t4, e'') = parse_Expr t3 in
        (t4, Ternary(e, e', e''))
    | _ -> (t, e)
and parse_OrExpr toks =
    let (t, e) = parse_AndExpr toks in
    match lookahead t with
    | Tok_Or ->
        let t' = match_token t Tok_Or in
        let (t'', e') = parse_OrExpr t' in
        (t'', Or(e, e'))
    | _ -> (t, e)
and parse_AndExpr toks =
    let (t, e) = parse_BitOrExpr toks in
    match lookahead t with
    | Tok_And ->
        let t' = match_token t Tok_And in
        let (t'', e') = parse_AndExpr t' in
        (t'', And(e, e'))
    | _ -> (t, e)
and parse_BitOrExpr toks =
    let (t, e) = parse_BitXorExpr toks in
    match lookahead t with
    | Tok_BitOr ->
        let t' = match_token t Tok_BitOr in
        let (t'', e') = parse_BitOrExpr t' in
        (t'', BitOr(e, e'))
    | _ -> (t, e)
and parse_BitXorExpr toks =
    let (t, e) = parse_BitAndExpr toks in
    match lookahead t with
    | Tok_BitXor ->
        let t' = match_token t Tok_BitXor in
        let (t'', e') = parse_BitXorExpr t' in
        (t'', BitXor(e, e'))
    | _ -> (t, e)
and parse_BitAndExpr toks =
    let (t, e) = parse_EqualityExpr toks in
    match lookahead t with
    | Tok_BitAnd ->
        let t' = match_token t Tok_BitAnd in
        let (t'', e') = parse_BitAndExpr t' in
        (t'', BitAnd(e, e'))
    | _ -> (t, e)
and parse_EqualityExpr toks =
    let (t, e) = parse_RelationalExpr toks in
    match lookahead t with
    | Tok_Equal ->
        let t' = match_token t Tok_Equal in
        let (t'', e') = parse_EqualityExpr t' in
        (t'', Equal(e, e'))
    | Tok_NotEqual ->
        let t' = match_token t Tok_NotEqual in
        let (t'', e') = parse_EqualityExpr t' in
        (t'', NotEqual(e, e'))
    | _ -> (t, e)
and parse_RelationalExpr toks =
    let (t, e) = parse_ShiftExpr toks in
    match lookahead t with
    | Tok_Less ->
        let t' = match_token t Tok_Less in
        let (t'', e') = parse_RelationalExpr t' in
        (t'', Less(e, e'))
    | Tok_Greater ->
        let t' = match_token t Tok_Greater in
        let (t'', e') = parse_RelationalExpr t' in
        (t'', Greater(e, e'))
    | Tok_LessEqual ->
        let t' = match_token t Tok_LessEqual in
        let (t'', e') = parse_RelationalExpr t' in
        (t'', LessEqual(e, e'))
    | Tok_GreaterEqual ->
        let t' = match_token t Tok_GreaterEqual in
        let (t'', e') = parse_RelationalExpr t' in
        (t'', GreaterEqual(e, e'))
    | _ -> (t, e)
and parse_ShiftExpr toks =
    let (t, e) = parse_AdditiveExpr toks in
    match lookahead t with
    | Tok_ShiftLeft ->
        let t' = match_token t Tok_ShiftLeft in
        let (t'', e') = parse_ShiftExpr t' in
        (t'', ShiftLeft(e, e'))
    | Tok_ShiftRight ->
        let t' = match_token t Tok_ShiftRight in
        let (t'', e') = parse_ShiftExpr t' in
        (t'', ShiftRight(e, e'))
    | _ -> (t, e)
and parse_AdditiveExpr toks =
    let (t, e) = parse_MultiplicativeExpr toks in
    match lookahead t with
    | Tok_Plus ->
        let t' = match_token t Tok_Plus in
        let (t'', e') = parse_AdditiveExpr t' in
        (t'', Add(e, e'))
    | Tok_Neg ->
        let t' = match_token t Tok_Neg in
        let (t'', e') = parse_AdditiveExpr t' in
        (t'', Sub(e, e'))
    | _ -> (t, e)
and parse_MultiplicativeExpr toks =
    let (t, e) = parse_UnaryExpr toks in
    match lookahead t with
    | Tok_Mul ->
        let t' = match_token t Tok_Mul in
        let (t'', e') = parse_MultiplicativeExpr t' in
        (t'', Mul(e, e'))
    | Tok_Div ->
        let t' = match_token t Tok_Div in
        let (t'', e') = parse_MultiplicativeExpr t' in
        (t'', Div(e, e'))
    | Tok_Mod ->
        let t' = match_token t Tok_Mod in
        let (t'', e') = parse_MultiplicativeExpr t' in
        (t'', Mod(e, e'))
    | _ -> (t, e)
and parse_UnaryExpr toks =
    match lookahead toks with
    | Tok_Neg ->
        let t = match_token toks Tok_Neg in
        let (t', e) = parse_UnaryExpr t in
        (t', Neg(e))
    | Tok_BitComp ->
        let t = match_token toks Tok_BitComp in
        let (t', e) = parse_UnaryExpr t in
        (t', BitComp(e))
    | Tok_Not ->
        let t = match_token toks Tok_Not in
        let (t', e) = parse_UnaryExpr t in
        (t', Not(e))
    | _ -> parse_PrimaryExpr toks
and parse_PrimaryExpr toks =
    match lookahead toks with
        | Tok_ID(x) -> (match_token toks (Tok_ID(x)), ID(x))
        | Tok_Int(x) -> (match_token toks (Tok_Int(x)), Constant(x))
        | Tok_LParen ->
            let t = match_token toks Tok_LParen in
            let (t', e) = parse_Expr t in
            let t'' = match_token t' Tok_RParen in
            (t'', e)
        | _ -> raise (InvalidInputException(Printf.sprintf
                "unexpected token found in parse_Expr %s" (string_of_token (lookahead toks))))

let rec parse_Statement toks : (token list) * stmt =
    match lookahead toks with
    | Tok_Return ->
        let t = match_token toks Tok_Return in
        let (t', e) = parse_Expr t in
        let t'' = match_token t' Tok_Semi in
        (t'', Return(e))
    | Tok_If ->
        let t = match_tokens toks [Tok_If; Tok_LParen] in
        let (t', e) = parse_Expr t in
        let t'' = match_token t' Tok_RParen in
        let (t3, s) = parse_Statement t'' in
        (match lookahead t3 with
        | Tok_Else ->
            let t4 = match_token t3 Tok_Else in
            let (t5, s') = parse_Statement t4 in
            (t5, Conditional(e, s, Some s'))
        | _ -> (t3, Conditional(e, s, None)))
    | _ ->
        let (t, e) = parse_Expr toks in
        let t' = match_token t Tok_Semi in
        (t', Expr(e))

let rec parse_Declaration toks : token list * declaration =
    let t = match_token toks Tok_Int_Type in
    match lookahead t with
    | Tok_ID(x) ->
        let t' = match_token t (Tok_ID(x)) in
        (match lookahead t' with
        | Tok_Assign ->
            let t'' = match_token t' Tok_Assign in
            let (t3, e) = parse_Expr t'' in
            let t4 = match_token t3 Tok_Semi in
            (t4, Declare(x, Some e))
        | Tok_Semi ->
            let t'' = match_token t' Tok_Semi in
            (t'', Declare(x, None))
        | _ -> raise (InvalidInputException(Printf.sprintf
                "unexpected token found in parse_Declaration %s" (string_of_token (lookahead t')))))
    | _ -> raise (InvalidInputException(Printf.sprintf
            "unexpected token found in parse_Declaration %s" (string_of_token (lookahead t))))

let rec parse_Block toks lst : (token list) * (block list) =
    match lookahead toks with
    | Tok_Int_Type ->
        let (t, d) = parse_Declaration toks in
        parse_Block t (lst@[Declaration(d)])
    | Tok_RBrace -> (toks, lst)
    | _ ->
        let (t, s) = parse_Statement toks in
        parse_Block t (lst@[Statement(s)])

let rec parse_Program toks =
    let (t,s) = parse_Function toks in
    (t, Program(s))
and parse_Function toks =
    let t = match_token toks Tok_Int_Type in
    match lookahead t with
    | Tok_ID(x) ->
        let t' = match_token t (Tok_ID(x)) in
        let t'' = match_tokens t' [Tok_LParen; Tok_RParen; Tok_LBrace] in
        let (t3, s) = parse_Block t'' [] in
        let t4 = match_token t3 Tok_RBrace in
        (t4, Function(x, s))
    | _ -> raise (InvalidInputException(Printf.sprintf
            "unexpected token found in parse_Function %s" (string_of_token (lookahead t))))

let parse_main toks =
    let (t, s) = parse_Program toks in
    let t' = match_token t EOF in s

