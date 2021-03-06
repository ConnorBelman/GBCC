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
    | Tok_Mod -> "Tok_Mod"
    | Tok_BitOr -> "Tok_BitOr"
    | Tok_BitAnd -> "Tok_BitAnd"
    | Tok_BitXor -> "Tok_BitXor"
    | Tok_ShiftLeft -> "Tok_ShiftLeft"
    | Tok_ShiftRight -> "Tok_ShiftRight"
    | Tok_Assign -> "Tok_Assign"
    | Tok_And -> "Tok_And"
    | Tok_Or -> "Tok_Or"
    | Tok_Equal -> "Tok_Equal"
    | Tok_NotEqual -> "Tok_NotEqual"
    | Tok_Greater -> "Tok_Greater"
    | Tok_GreaterEqual -> "Tok_GreaterEqual"
    | Tok_Less -> "Tok_Less"
    | Tok_LessEqual -> "Tok_LessEqual"
    | Tok_PlusEqual -> "Tok_PlusEqual"
    | Tok_MinEqual -> "Tok_MinEqual"
    | Tok_DivEqual -> "Tok_DivEqual"
    | Tok_MulEqual -> "Tok_MulEqual"
    | Tok_ModEqual -> "Tok_ModEqual"
    | Tok_LShiftEqual -> "Tok_LShiftEqual"
    | Tok_RShiftEqual -> "Tok_RShiftEqual"
    | Tok_AndEqual -> "Tok_AndEqual"
    | Tok_OrEqual -> "Tok_OrEqual"
    | Tok_XorEqual -> "Tok_XorEqual"
    | Tok_If -> "Tok_If"
    | Tok_Else -> " Tok_Else"
    | Tok_Colon -> "Tok_Colon"
    | Tok_Question -> "Tok_Question"
    | Tok_For -> "Tok_For"
    | Tok_While -> "Tok_While"
    | Tok_Do -> "Tok_Do"
    | Tok_Break -> "Tok_Break"
    | Tok_Continue -> "Tok_Continue"
    | EOF -> "EOF"

let string_of_list ?newline:(newline=false) (f : 'a -> string) (l : 'a list) : string =
    "[" ^ (String.concat ", " @@ List.map f l) ^ "]" ^ (if newline then "\n" else "");;

