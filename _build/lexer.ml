(* open Batteries;; *)

(* Lexer *)

type token =
    (* Commands *)
    | Def | Extern

    (* Primary *)
    | Ident of string | Number of float

    (* unknown, aka keyword *)
    | Kwd of char
;;

let string_of_token (t : token) : string =
    match t with
    | Def -> "def"
    | Extern -> "extern"
    | Ident s -> ("Ident " ^ s)
    | Number n -> ("Number " ^ Batteries.String.of_float n)
    | Kwd c -> ("Other token " ^ Batteries.String.of_char c)
;;

(* parse until the end of the line *)
let rec parse_comment (file_chars : char list) : char list =
    (* parse until newline char or end of file *)
    match file_chars with
    | [] -> []
    | hd :: tl -> if Batteries.Char.is_newline hd then tl else parse_comment tl
;;

(* given the first char of an identifier, parse the rest of the token
 * return the Ident token *)
let parse_ident (file_chars : char list) (ident_name: string) : (token * char list) =
    (* a valid identifier char is a letter, digit, or _ *)
    let is_valid_ident_char (c : char) : bool =
        Batteries.Char.is_letter c || Batteries.Char.is_digit c || c = '_' in 

    (* read until an invalid char is found *) 
    let rec helper (input_chars : char list) (acc_str: string) : (string * char list) =
        match input_chars with 
        | [] -> (acc_str, [])
        | next_char :: tl -> 
            if is_valid_ident_char next_char then
                let s = acc_str ^ (Batteries.String.of_char next_char) in
                helper tl s
            else (acc_str, input_chars)
    in
    let tok_str, updated_list = helper file_chars ident_name in
    let tok = 
    if tok_str = "def" then Def
    else if tok_str = "extern" then Extern
    else Ident tok_str
    in 
    (tok, updated_list)
;;

(* Given the first number of a float, parse the number 
 * For now, don't make a fuss if the number is invalid *)
let rec parse_num (file_chars : char list) (num_str: string) : (token * char list) =
    let is_valid_num_char (c: char) : bool = Batteries.Char.is_digit c || c = '.' in
    let get_num str = Batteries.String.to_float str in 

    (* read chars until the first invalid num char is found *)
    match file_chars with
    | [] ->
        (Number (get_num num_str), file_chars)
    | next_char :: tl -> 
        if is_valid_num_char next_char then
            let n = num_str ^ (Batteries.String.of_char next_char) in
            parse_num tl n
        else
            (Number (get_num num_str), file_chars)
;;

(* Parse the entire file 
 * TODO: make this tail-recursive, or stack with collapse under the burden of tokens
 *)
let rec parse (file_chars: char list) (tokens: token list) : token list =
    match file_chars with
    | [] -> tokens
    | next_char :: tl -> 
        (* whitespace *)
        if Batteries.Char.is_whitespace next_char then
            parse tl tokens

        (* comment *)
        else if next_char = '#' then
            let new_list = parse_comment file_chars in
            parse new_list tokens

        (* identifier *)
        else if Batteries.Char.is_letter next_char then (
            let str_start : string = Batteries.String.of_char next_char in 
            let tok, new_list = parse_ident tl str_start in
            parse new_list (tok :: tokens); 
        )

        (* number *)
        else if Batteries.Char.is_digit next_char then (
            let str_start : string = Batteries.String.of_char next_char in
            let tok, new_list = parse_num tl str_start in
            parse new_list (tok :: tokens);
        ) 

        (* Other char *)
        else (
            let tok = Kwd next_char in
            parse tl (tok :: tokens)
        ) 
;;

(* print all tokens, for debugging *)
let print_tokens (tokens : token list) : unit =
    List.iter (fun a -> print_endline (string_of_token a)) tokens
;; 

(* read all of the characters of the input file *)
let read_all_chars (file : in_channel) : char list = 
    let rec reader_helper (acc : char list) : char list = 
        try 
            let c = input_char file in
            reader_helper (c :: acc) 
        with 
            | End_of_file -> print_endline "reached end of file"; acc
    in 
    Batteries.List.rev (reader_helper [])
;;

(* Open the src file *)
let tokenize_channel (channel : in_channel) : token list =
    let all_chars : char list = read_all_chars channel in
    close_in channel;
    let tokens: token list = Batteries.List.rev (parse all_chars []) in
    tokens
;;

let tokenize_file (file_name : string) : token list =
  let channel = open_in file_name in
  let tokens = tokenize_channel channel in
  close_in channel;
  tokens

(* For the toplevel *)
let tokenize_string (input : string) : token list =
  let all_chars = Batteries.String.to_list input in
  let tokens: token list = Batteries.List.rev (parse all_chars []) in
  tokens
