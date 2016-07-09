type expr =
    | Number of float
    | Variable of string
    | Binary of char * expr * expr
    | Call of string * expr array

(* Description of a function. Takes the name and the argument names *)
type proto = Prototype of string * string array

(* Description of the function along with the function defintion/expression *)
type func = Function of proto * expr

(* Define the precedence of binary operators *)
let binop_precedence: (char, int) Hashtbl.t = Hashtbl.create 10;;
let precedence c = try Hashtbl.find binop_precedence c with Not_found -> -1;;

Hashtbl.add binop_precedence '<' 10;;
Hashtbl.add binop_precedence '+' 20;;
Hashtbl.add binop_precedence '-' 20;;
Hashtbl.add binop_precedence '*' 40;;

(* Parse the tokens after a function def (not including "def"), and return
the expression and remaining tokens *)
let parse_prototype (tokens: Lexer.token list) : (proto * Lexer.token list) =

  (* Parse the args from opening '(' (not included in arg_tokens) to the ending ')' (included in arg tokens) *)
  let rec parse_args (acc_args: string list) (arg_tokens: Lexer.token list) : (string list * Lexer.token list) =
    match arg_tokens with 
    | Lexer.Kwd ')' :: tl -> (List.rev acc_args, tl)
    | Lexer.Ident s :: tl -> parse_args (s :: acc_args) tl
    | _ -> failwith "Error parsing prototype args"
  in
  match tokens with 
  | Lexer.Ident s :: Lexer.Kwd '(' :: tl ->
      let (args, remaining_tokens) = parse_args [] tl in
      let p = Prototype (s, Array.of_list args) in
      (p, remaining_tokens) 
  | _ -> failwith "Incorrect prototype format"
;;

(* Generate the AST from the token list *)
let rec parse_primary (tokens: Lexer.token list) : (expr * Lexer.token list) =
  match tokens with 
  | [] -> failwith "ended in unexpected location"
  | Lexer.Number n :: tl -> (Number n, tl)
  | Lexer.Ident s :: tl -> 
      begin
      match tl with
      | Lexer.Kwd '(' :: tl_b -> parse_call tokens 
      | _ ->  (Variable s, tl)
      end
  | Lexer.Kwd '(' :: tl ->
      begin
      let (expr, remaining) = parse_expr tl in
      match remaining with
      | Lexer.Kwd ')' :: tl_b -> (expr, tl_b)
      | _ -> failwith "expected a ')'"
      end
  | Lexer.Kwd c :: tl -> failwith ("did not expect char " ^ Batteries.String.of_char c)
  | Lexer.Extern :: tl -> failwith "not expecting extern"
  | Lexer.Def :: tl -> failwith "not expected def"
and 
(*
  Parse a function (starting with def)
 *)
parse_function (tokens: Lexer.token list) : (func * Lexer.token list) =
  match tokens with 
  | Lexer.Def :: tl -> 
      let (proto, body_tokens) = parse_prototype tl in
      let (body_expr, remaining_tokens) = parse_expr body_tokens in
      let f = Function (proto, body_expr) in
      (f, remaining_tokens)
  | _ -> failwith "Incorrect function format"
and
(*
  Parse a function call, with the first token being the function name
 *)
parse_call (tokens: Lexer.token list) : (expr * Lexer.token list) =
  
  (* Parse the args from opening '(' (not included) to ending ')' (included) *)
  let rec parse_args (acc_args: expr list) (arg_tokens: Lexer.token list) : (expr list * Lexer.token list) =
    let (arg, remaining) = parse_expr arg_tokens in
    let new_list = arg :: acc_args in
    match remaining with
    | Lexer.Kwd ')' :: tl -> (List.rev new_list, tl) 
    | Lexer.Kwd ',' :: tl -> parse_args new_list tl
    | _ -> failwith "Incorrect call arguments format"
  in
  match tokens with
  | Lexer.Ident name :: Lexer.Kwd '(' :: Lexer.Kwd ')' :: tl -> 
    let call = Call (name, [||]) in
    (call, tl)
  | Lexer.Ident name :: Lexer.Kwd '(' :: tl -> 
      let (args, remaining_tokens) = parse_args [] tl in
      let call = Call (name, (Array.of_list args)) in
      (call, remaining_tokens)
  | _ -> failwith "Incorrect call format"
and 
(*
  Recursively construct a binary expression
*)
parse_binary_rhs (expr_prec: int) (lhs: expr) (tokens: Lexer.token list) : (expr * Lexer.token list) =
  begin
  match tokens with 
  | Lexer.Kwd op_a :: tl ->
    let prec_a = precedence op_a in
    if prec_a < expr_prec then
      (lhs, tokens)
    else
      (* for a o1 b o2 ..., parse as (a o1 b) o2 ... or a o1 (b o2 ...), where o1 and o2 are bin operators *)
      let (next_expr, remaining) = parse_primary tl in
      let (rhs, remaining_b) = 
        match remaining with
        | Lexer.Kwd op_b :: tl -> 
            begin
            if precedence op_b < prec_a then
              (* Case (a o1 b) o2 ... RHS is b *)
              (next_expr, remaining)
            else 
              (* Case a o1 (b o2 ...) RHS is (b o2 ...) *)
              parse_binary_rhs prec_a next_expr remaining
            end
        | _ -> (next_expr, remaining) (* Case (a o1 b), end of binary expr *)
      in
      let binary_expr = Binary (op_a, lhs, rhs) in
      parse_binary_rhs expr_prec binary_expr remaining_b
  | _ -> (lhs, tokens) (* end of binary expression *)
  end
and
(* start the recursion *)
parse_expr (tokens: Lexer.token list) : (expr * Lexer.token list) =
  let (lhs, remaining) = parse_primary tokens in
  parse_binary_rhs 0 lhs remaining

let parse_toplevel (name : string) (tokens: Lexer.token list) : (func * Lexer.token list) = 
  let (ex, remaining) = parse_expr tokens in
  let f = Function (Prototype (name, [||]), ex) in
  (f, remaining)

(* Helpers *)

let rec string_of_expr (e : expr) =
  match e with
  | Number n -> (Batteries.String.of_float n)
  | Variable s -> s
  | Binary (op, l, r) ->
      let op_string = Batteries.String.of_char op in
      let left_string = string_of_expr l in
      let right_string = string_of_expr r in
      "(" ^ op_string ^ ", " ^ left_string ^ ", " ^ right_string ^ ")"
  | Call (name, exprs) -> 
      let combine (acc: string) expr : string =
        acc ^ (string_of_expr expr) ^ " "
      in
      let args_string = List.fold_left combine "" (Array.to_list exprs) in
      "Call " ^ name ^ " with " ^ args_string
;;

let string_of_proto (p: proto) : string =
  let combine = fun e acc -> (e ^ " " ^ acc) in
  match p with
  | Prototype (name, args) -> name ^ " " ^ (Array.fold_right combine args "")
;;

let string_of_func (f : func) : string = 
  match f with
  | Function (proto, expr) -> "def " ^ string_of_proto proto ^ " -> " ^ (string_of_expr expr)
;;
