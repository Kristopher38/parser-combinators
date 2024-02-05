module Ast = struct
  type const =
  | CInt of int
  | CFloat of float
  | CString of string

  type identifier = string
  type binop =
  | Plus
  | Minus
  | Mul
  | Div

  type unop =
  | UMinus
  | Neg

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

  let reserved = ["let"; "in"; "fun"; "if"; "then"; "else"]
  let identifier =
    let alpha_underscore = alpha ++ (char '_') in
    let* first = alpha_underscore in
    let* rest = many (alpha_underscore ++ digit) in
    let str = (first :: rest) |> to_string in
    if List.mem str reserved then
      zero
    else
      return (first :: rest)

  let const =
    let number_parser =
      let* _number = option (str "-") @@ (many1 digit) in
      return (CInt(to_string _number |> int_of_string))
    and float_parser =
      let* _float = (option (str "-")) @@ (many1 digit << char '.' >> many digit) in
      return (CFloat(to_string _float |> float_of_string))
    and string_parser =
      let printable_char = sat (in_range '\x23' '\x7f') ++ sat (in_range '\x20' '\x21') in
      let* _string = delim (char '"') (many printable_char) (char '"') in
      return (CString(to_string _string))
    in choice [
      float_parser;
      number_parser;
      string_parser;
    ]

  let binop = str_set ["+"; "-"; "*"; "/"](* ; "+."; "-."; "*."; "/."; "<"; "<="; ">"; ">="; "=="; "~="; "&"; "|"; "^"; "&&"; "||"; "::"; ".."; ","] *)
  let unop = str_set ["-"; "!"](*; "hd"; "tl"; "fst"; "snd"]*)

  let binop_m = [
    ("+", Plus);
    ("-", Minus);
    ("*", Mul);
    ("/", Div);
  ]
  let unop_m = [
    ("-", UMinus);
    ("!", Neg);
  ]
  let arg_list = sepby1 (token identifier) (token (char ','))

  let defer p = 
    (return ()) >>= (fun _ -> p ())
  let (!:) = defer

  let operator_parser term prefix infix =
    let rec parse_prefix () =
      let* op, rbp = prefix in
      let* rhs = parse rbp in
      return (op rhs)
    and parse bp =
      let* lhs = !:parse_prefix ++ term in
      parse_infix_loop lhs bp ++ return lhs
    and parse_infix_loop lhs bp =
      let* new_lhs = parse_infix lhs bp in
      parse_infix_loop new_lhs bp ++ return new_lhs
    and parse_infix lhs bp =
      let* op, lbp, rbp = infix in
      if lbp >= bp then
        let* rhs = parse rbp in
        return (op lhs rhs)
      else
        zero
    in parse 0


  let map p retval = p >>= fun _ -> return retval
  
  let prefix = choice [
    (map (char '-') ((fun a -> Unop(UMinus, a)), 4));
    (map (char '!') ((fun a -> Unop(Neg, a)), 4));
  ]
  let infix = choice [
    (map (char '*') ((fun a b -> Binop(a, Mul, b)), 2, 3));
    (map (char '/') ((fun a b -> Binop(a, Div, b)), 2, 3));
    (map (char '+') ((fun a b -> Binop(a, Plus, b)), 3, 2));
    (map (char '-') ((fun a b -> Binop(a, Minus, b)), 3, 2));
  ]

  let rec expr () =
    let let_expr =
      let* let_id = token (str "let") >> token identifier << token (char '=') in
      let* let_val = !:expr << token (str "in") in
      let* let_expr = !:expr in
      let* e' = !:expr' in
      return (e' (Let(to_string let_id, let_val, let_expr)))
    and unop_expr =
      let* op = unop in
      let* _expr = !:expr in
      let* e' = !:expr' in
      return (e' (Unop(List.assoc (to_string op) unop_m, _expr)))
    and fundecl_expr =
      let* fun_args = token (str "fun") >> arg_list << token (str "->") in
      let* fun_body = !:expr in
      let* e' = !:expr' in
      return (e' (FunDecl(List.map to_string fun_args, fun_body)))
    and const_expr =
      let* value = token const in
      let* e' = !:expr' in
      return (e' (Const(value)))
    and list_expr =
      let* elems = token (char '[') >> (sepby (!:expr) (token (char ';'))) << token (char ']') in
      let* e' = !:expr' in
      return (e' (ListExpr(elems)))
    and paren_expr =
      let* e = token (char '(') >> !:expr << token (char ')') in
      let* e' = !:expr' in
      return (e' e)
    and if_expr =
      let* cond = token  (str "if") >> !:expr << token (str "then") in
      let* then_expr = !:expr << token (str "else") in
      let* else_expr = !:expr in
      let* e' = !:expr' in
      return (e' (If(cond, then_expr, else_expr)))
    and id_expr =
      let* id = token identifier in
      let* e' = !:expr' in
      return (e' (Id(to_string id)))
    and binop_expr = operator_parser !:expr prefix infix
    in choice [
      id_expr;
      let_expr;
      fundecl_expr;
      unop_expr;
      const_expr;
      list_expr;
      paren_expr;
      if_expr;
      binop_expr;
    ]
  and expr' () =
    let (*binop_expr =
      let* op = token binop in
      let* rhs = !:expr in
      return (fun x -> Binop(x, List.assoc (to_string op) binop_m, rhs))
    and *) app_expr =
      let* arg = !:expr in
      return (fun x -> App(x, arg))
    in (*binop_expr ++*) app_expr ++ return (fun x -> x)

  (* type ('a, 'b) operator =
  | InfixL of (('a -> 'b -> 'b) * int * int) Parser_combinators.Combinators.ParserMonad.t
  | InfixR of (('a -> 'b -> 'b) * int * int) Parser_combinators.Combinators.ParserMonad.t
  | Prefix of (('a -> 'a) * int) Parser_combinators.Combinators.ParserMonad.t *)

  (* let make_table ops =
    List.mapi (fun i xs -> (List.map (fun x -> 
      (match x with
      | InfixL t -> t
      | InfixR t -> t
      | Prefix t -> t), i * 2) xs)) ops *)

  (* let tbl = [
    [
      Prefix(map (char '-') UMinus);
      Prefix(map (char '!') Neg);
    ];
    [
      InfixL(map (char '*') Mul);
      InfixL(map (char '/') Div);
    ];
    [
      InfixL(map (char '+') Plus);
      InfixL(map (char '-') Minus);
    ]
  ] *)


  let prog = many (expr ()) << eof
  let parse str = run prog (of_string str)
end


let read_file file = In_channel.with_open_bin file In_channel.input_all

let rev_assoc xs =
  let ys, zs = List.split xs in List.combine zs ys
let rec ast_to_string e = match e with
| Ast.Let(id, e1, e2) -> "let " ^ id ^ " = " ^ ast_to_string  e1 ^ " in " ^ ast_to_string  e2
| Ast.Binop(e1, op, e2) -> ast_to_string  e1 ^ " " ^ List.assoc op (rev_assoc LangParser.binop_m) ^ " " ^ ast_to_string  e2
| Ast.Unop(op, e) -> List.assoc op (rev_assoc LangParser.unop_m) ^ ast_to_string e
| Ast.FunDecl(xs, e) -> "fun " ^ String.concat "," xs ^ " -> " ^ ast_to_string e
| Ast.Const(CInt n) -> string_of_int n
| Ast.Const(CFloat f) -> string_of_float f
| Ast.Const(CString s) -> "\"" ^ s ^ "\""
| Ast.ListExpr(xs) -> String.concat "\n" (List.map ast_to_string xs)
| Ast.App(e1, e2) -> ast_to_string e1 ^ " " ^ ast_to_string e2
| Ast.If(cond, _then, _else) -> "if " ^ ast_to_string cond ^ " then " ^ ast_to_string _then ^ " else " ^ ast_to_string _else
| Ast.Id(id) -> id

let program = match LangParser.parse (read_file Sys.argv.(1)) with
| Some xs -> String.concat "\n" (List.map ast_to_string xs)
| None -> "fail"

let _ = print_endline program
