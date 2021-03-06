type expr =
    | Number of float
    | Variable of string
    | Binary of char * expr * expr
    | Call of string * expr array
    | If of expr * expr * expr
    | For of string * expr * expr * expr option * expr

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
  | Lexer.If :: tl -> parse_if tokens
  | Lexer.For :: tl -> parse_for tokens
  | Lexer.Kwd c :: tl -> failwith ("did not expect char " ^ Batteries.String.of_char c)
  | Lexer.Extern :: tl -> failwith "not expecting extern"
  | Lexer.Def :: tl -> failwith "not expected def"
  | hd :: tl -> failwith ("unexpected token when parsing primary: " ^ (Lexer.string_of_token hd))
and 
(*
Parse an for-in
*)
parse_for (tokens : Lexer.token list) : (expr * Lexer.token list) =
  let parse_init (tokens : Lexer.token list) :  (string * expr * Lexer.token list) =
    match tokens with
    | Lexer.For :: Lexer.Ident s :: Lexer.Kwd '=' :: tl ->
        let (init_expr, remaining) = parse_expr tl in
        (s, init_expr, remaining)
    | _ -> failwith "error in parsing first segment of for loop"
  in 
  let parse_end_cond (tokens : Lexer.token list) : (expr * Lexer.token list) =
    match tokens with
    | Lexer.Kwd ',' :: tl -> parse_expr tl
    | _ -> failwith "expected ',' after initialize segment of for-loop"
  in
  let parse_inc_expr (tokens : Lexer.token list) : (expr option * Lexer.token list) = 
    match tokens with 
    | Lexer.Kwd ',' :: tl -> 
        let (expr, remaining) = parse_expr tl in
        (Some expr, remaining)
    | _ -> (None, tokens)
  in
  let (inc_name, init_expr, remaining) = parse_init tokens in
  let (end_cond, remaining) = parse_end_cond remaining in
  let (opt_inc_expr, remaining) = parse_inc_expr remaining in
  let (body_expr, remaining) = parse_expr remaining in
  let expr = For (inc_name, init_expr, end_cond, opt_inc_expr, body_expr) in
  (expr, remaining)
and     
(* 
 Parse an if-then-else
*)
parse_if (tokens : Lexer.token list) : (expr * Lexer.token list) =
  begin
  match tokens with
  | Lexer.If :: tl ->
      let (cond, remaining) = parse_expr tl in
      begin
      match remaining with
      | Lexer.Then :: tl -> 
          let (exprTrue, remaining) = parse_expr tl in
          begin
          match remaining with
          | Lexer.Else :: tl -> 
              let (exprFalse, remaining) = parse_expr tl in
              (If (cond, exprTrue, exprFalse), remaining)
          | _ -> failwith "expected 'else'"
          end
      | _ -> failwith "expected 'then'"
      end
  | _ -> failwith "expected 'if'"
  end
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
  Parse an extern *)
parse_extern (tokens: Lexer.token list) : (proto * Lexer.token list) =
  match tokens with
  | Lexer.Extern :: tl ->
      let (proto, remaining) = parse_prototype tl in
      (proto, remaining)
  | _ -> failwith "incorrect extern format"
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
  | If (cond, expTrue, expFalse) ->
      let cond_str = string_of_expr cond in
      let a_str = string_of_expr expTrue in
      let b_str = string_of_expr expFalse in
      "if " ^ cond_str ^ "\nthen " ^ a_str ^ "\nelse " ^ b_str
  | For (ident, init_expr, end_cond, opt_inc_expr, body_expr) ->
      let init_str = string_of_expr init_expr in
      let end_str = string_of_expr end_cond in
      let inc_str = match opt_inc_expr with | None -> "" | Some e -> (", " ^ string_of_expr e) in
      let body_str = string_of_expr body_expr in
      "for " ^ ident ^ " = " ^ init_str ^ ", " ^ end_str ^ inc_str ^ ": " ^ body_str 
;;

let string_of_proto (p: proto) : string =
  let combine = fun e acc -> (e ^ " " ^ acc) in
  match p with
  | Prototype (name, args) -> name ^ " " ^ (Array.fold_right combine args "")

let string_of_func (f : func) : string = 
  match f with
  | Function (proto, expr) -> "def " ^ string_of_proto proto ^ " -> " ^ (string_of_expr expr)

let string_of_extern (p : proto) : string =
  "extern " ^ (string_of_proto p)
