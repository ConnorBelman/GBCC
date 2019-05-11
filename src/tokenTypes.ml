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
    | Tok_Mod
    | Tok_And
    | Tok_Or
    | Tok_Equal
    | Tok_NotEqual
    | Tok_Greater
    | Tok_GreaterEqual
    | Tok_Less
    | Tok_LessEqual
    | Tok_Assign
    | Tok_PlusEqual
    | Tok_MinEqual
    | Tok_DivEqual
    | Tok_MulEqual
    | Tok_ModEqual
    | Tok_LShiftEqual
    | Tok_RShiftEqual
    | Tok_AndEqual
    | Tok_OrEqual
    | Tok_XorEqual
    | Tok_If
    | Tok_Else
    | Tok_Colon
    | Tok_Question
    | Tok_For
    | Tok_While
    | Tok_Do
    | Tok_Break
    | Tok_Continue
    | EOF
