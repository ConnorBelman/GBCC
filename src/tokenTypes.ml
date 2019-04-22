exception InvalidInputException of string

type token =
    | Tok_LBrace
    | Tok_RBrace
    | Tok_LParen
    | Tok_RParen
    | Tok_Semi
    | Tok_Int_Type
    | Tok_Return
    | Tok_ID of string
    | Tok_Int of int
    | Tok_Neg
    | Tok_BitComp
    | Tok_BitOr
    | Tok_BitAnd
    | Tok_BitXor
    | Tok_ShiftLeft
    | Tok_ShiftRight
    | Tok_Not
    | Tok_Plus
    | Tok_Mul
    | Tok_Div
    | Tok_And
    | Tok_Or
    | Tok_Equal
    | Tok_NotEqual
    | Tok_Greater
    | Tok_GreaterEqual
    | Tok_Less
    | Tok_LessEqual
    | EOF
