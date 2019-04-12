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
    | EOF
