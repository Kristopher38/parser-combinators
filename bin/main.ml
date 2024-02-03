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
  | FunDecl of identifier list * expr
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

  let binop = str_set ["+"; "-"; "*"; "/"; "+."; "-."; "*."; "/."; "<"; "<="; ">"; ">="; "=="; "~="; "&"; "|"; "^"; "&&"; "||"; "::"; ".."; ","]
  let unop = str_set ["-"; "!"; "hd"; "tl"; "fst"; "snd"]
  let arg_list = sepby1 (str ",") identifier

  let rec expr () =
    let let_expr =
      let* let_id = str "let" >> identifier << char '=' in
      let* let_val = expr () << str "in" in
      let* let_expr = expr () in
      let* e' = expr' () in
      return (e' (Let(to_string let_id, let_val, let_expr)))
    and unop_expr =
      let* op = unop in
      let* _expr = expr () in
      let* e' = expr' () in
      return (e' (Unop(to_string op, _expr)))
    and fundecl_expr =
      let* fun_args = str "fun" >> arg_list << str "->" in
      let* fun_body = expr () in
      let* e' = expr' () in
      return (e' (FunDecl(List.map to_string fun_args, fun_body)))
    and const_expr =
      let* value = const in
      let* e' = expr' () in
      return (e' (Const(value)))
    and list_expr =
      let* _ = char '[' in
      let* elems = (sepby (expr ()) (char ';')) << char ']' in
      let* e' = expr' () in
      return (e' (ListExpr(elems)))
    and paren_expr =
      let* _ = char '(' in
      let* e = (expr ()) << (char ')') in
      let* e' = expr' () in
      return (e' e)
    and if_expr =
      let* _ = str "if" in
      let* cond = expr () << str "then" in
      let* then_expr = expr () << str "else" in
      let* else_expr = expr () in
      let* e' = expr' () in
      return (e' (If(cond, then_expr, else_expr)))
    and id_expr =
      let* id = identifier in
      let* e' = expr' () in
      return (e' (Id(to_string id)))
    in choice [
      let_expr;
      unop_expr;
      fundecl_expr;
      const_expr;
      list_expr;
      paren_expr;
      if_expr;
      id_expr;
    ]
  and expr' () =
    let binop_expr =
      let* op = binop in
      let* rhs = expr () in
      return (fun x -> Binop(x, to_string op, rhs))
    and app_expr =
      let* arg = expr () in
      return (fun x -> App(x, arg))
    in binop_expr ++ app_expr ++ return (fun x -> x)

  let prog = many (expr ())
  let parse str = run prog (of_string str)
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