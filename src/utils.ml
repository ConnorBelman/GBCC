open CTypes
open TokenTypes

let string_of_token (t : token) : string = match t with
    | Tok_Int_Type -> "Tok_Int_Type"
    | Tok_Semi -> "Tok_Semi"
    | Tok_RParen -> "Tok_RParen"
    | Tok_RBrace -> "Tok_RBrace"
    | Tok_LParen -> "Tok_LParen"
    | Tok_LBrace-> "Tok_LBrace"
    | Tok_Int(i) -> "Tok_Int(" ^ (string_of_int i) ^ ")"
    | Tok_ID(id) -> "Tok_ID(\"" ^ id ^ "\")"
    | Tok_Return -> "Tok_Return"
    | Tok_Neg -> "Tok_Neg"
    | Tok_BitComp -> "Tok_BitComp"
    | Tok_Not -> "Tok_Not"
    | Tok_Plus -> "Tok_Plus"
    | Tok_Mul -> "Tok_Mul"
    | Tok_Div -> "Tok_Div"
    | Tok_BitOr -> "Tok_BitOr"
    | Tok_BitAnd -> "Tok_BitAnd"
    | Tok_BitXor -> "Tok_BitXor"
    | Tok_ShiftLeft -> "Tok_ShiftLeft"
    | Tok_ShiftRight -> "Tok_ShiftRight"
    | EOF -> "EOF"

let string_of_list ?newline:(newline=false) (f : 'a -> string) (l : 'a list) : string =
    "[" ^ (String.concat ", " @@ List.map f l) ^ "]" ^ (if newline then "\n" else "");;

