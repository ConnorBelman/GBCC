open CTypes
open TokenTypes

let test_tokenize toks = List.rev @@ EOF::toks
