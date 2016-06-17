type expr =
    | Number of float
    | Variable of string
    | Binary of char * expr * expr
    | Call of string * expr array

(* Description of a function. Takes the name and the argument names *)
type proto = Prototype of string * string array

(* Description of the function along with the function defintion/expression *)
type func = Function of proto * expr

(* Generate the AST from the token list *)
let rec generate_ast (tokens: Lexer.token list) (ast : expr) : expr =
    match tokens with 
    | [] -> ast
    | hd :: tl ->
        match hd with 
        | Def -> (* TODO parse function declaration and body *)
        | Extern -> (* TODO parse the extern... <- maybe just get rid of externs, what are they for anyways? *)
        | _ -> print_endline "Unexpected token found at toplevel"
;;
