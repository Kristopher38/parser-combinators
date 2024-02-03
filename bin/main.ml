module Ast = struct
  type const =
  | CInt of int
  | CFloat of float
  | CString of string

  type identifier = string
  type binop = string
  type unop = string

  type expr =
  | Let of identifier * expr * expr
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | FunDecl of expr list * expr
  | Const of const
  | ListExpr of expr list
  | App of expr * expr
  | If of expr * expr * expr
  | Id of identifier
end

module LangParser = struct
  open Parser_combinators.Combinators.ParserMonad
  open Ast
  let identifier =
    let alpha_underscore = alpha ++ (char '_') in
    let* first = alpha_underscore in
    let* rest = many1 (alpha_underscore ++ digit) in
    return (first :: rest)

  let const =
    let* _number = option (str "-") @@ (many digit) in
    let number = return (CInt(to_string _number |> int_of_string)) in
    let* _float = (option (str "-")) @@ (many1 digit << char '.' >> many digit) in
    let float = return (CFloat(to_string _float |> float_of_string)) in
    let* _string = delim (str "\"") (many (sat (in_range '\x20' '\x7f'))) (str "\"") in
    let string = return (CString(to_string _string)) in
    number ++ float ++ string

  let binop = str_set ["="; "-"; "*"; "/"; "+."; "-."; "*."; "/."; "<"; "<="; ">"; ">="; "=="; "~="; "&"; "|"; "^"; "&&"; "||"; "::"; ".."; ","]
  let unop = str_set ["-"; "!"; "hd"; "tl"; "fst"; "snd"]
  let arg_list = sepby1 (str ",") identifier
end

(* let nat =
  let num c = (int_of_char c) - (int_of_char '0') in
  let eval str = List.fold_left (fun acc x -> 10 * acc + num x) 0 str in
  let* digits = many1 digit in
  return (eval digits) *)

(* let dig = PM.run PM.int_list (PM.of_string "[45,-36,75,-222]") 

let _ = match dig with
| Some list -> List.iter (Printf.printf "%d,") list
| None -> Printf.printf "fail" *)