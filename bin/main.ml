module Ast = struct
  type const =
  | CInt of int
  | CFloat of float
  | CString of string

  type identifier = string

  type binop =
  | Eq
  | LEq
  | GEq
  | Lt
  | Gt
  | NEq
  | And
  | Or
  | Cons
  | Concat
  | Plus
  | Minus
  | Mul
  | Div
  | Pow

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
      (* Note: missing special chars escaping *)
      let printable_char = sat (in_range '\x23' '\x7f') ++ sat (in_range '\x20' '\x21') in
      let* _string = delim (char '"') (many printable_char) (char '"') in
      return (CString(to_string _string))
    in choice [
      float_parser;
      number_parser;
      string_parser;
    ]

  let arg_list = sepby1 (token identifier) (token (char ','))

  let unop x = fun a -> Unop(x, a)
  let binop x = fun a b -> Binop(a, x, b)

  let ops = [
    [Infix(token (str "||"), binop Or, Right)];
    [Infix(token (str "&&"), binop And, Right)];
    [
      Infix(token (str "=="), binop Eq, Left);
      Infix(token (str "<"), binop Lt, Left);
      Infix(token (str ">"), binop Gt, Left);
      Infix(token (str "<="), binop LEq, Left);
      Infix(token (str ">="), binop GEq, Left);
      Infix(token (str "~="), binop NEq, Left)
    ];
    [Infix(token (str ".."), binop Concat, Right)];
    [Infix(token (str "::"), binop Cons, Right)];
    [
      Infix(token (str "+"), binop Plus, Left);
      Infix(token (str "-"), binop Minus, Left);
    ];
    [
      Infix(token (str "*"), binop Mul, Left);
      Infix(token (str "/"), binop Div, Left);
    ];
    [Infix(token (str "^"), binop Pow, Right)];
    [Infix(token (str "."), (fun a b -> App(a, b)), Left)];
    [Prefix(token (str "-"), unop UMinus)];
    [Prefix(token (str "!"), unop Neg)];
  ]

  let rec expr () =
    let let_expr =
      let* let_id = token (str "let") >> token identifier << token (char '=') in
      let* let_val = !:op_expr << token (str "in") in
      let* let_expr = !:op_expr in
      return (Let(to_string let_id, let_val, let_expr))
    and fundecl_expr =
      let* fun_args = token (str "fun") >> arg_list << token (str "->") in
      let* fun_body = !:op_expr in
      return (FunDecl(List.map to_string fun_args, fun_body))
    and const_expr =
      let* value = token const in
      return (Const(value))
    and list_expr =
      let* elems = token (char '[') >> (sepby (!:op_expr) (token (char ';'))) << token (char ']') in
      return (ListExpr(elems))
    and paren_expr =
      token (char '(') >> !:op_expr << token (char ')')
    and if_expr =
      let* cond = token  (str "if") >> !:op_expr << token (str "then") in
      let* then_expr = !:op_expr << token (str "else") in
      let* else_expr = !:op_expr in
      return (If(cond, then_expr, else_expr))
    and id_expr =
      let* id = token identifier in
      return (Id(to_string id))
    in choice [
      id_expr;
      let_expr;
      fundecl_expr;
      const_expr;
      list_expr;
      paren_expr;
      if_expr;
    ]
  and op_expr () =
    operator_parser !:expr ops

  let prog = many (op_expr ()) << eof
  let parse str = run prog (of_string str)
end


let read_file file = In_channel.with_open_bin file In_channel.input_all

let binop_m = [
  (Ast.Plus, "+");
  (Ast.Minus, "-");
  (Ast.Mul, "*");
  (Ast.Div, "/");
  (Ast.Pow, "^");
  (Ast.Eq, "==");
  (Ast.LEq, "<=");
  (Ast.GEq, ">=");
  (Ast.Lt, "<");
  (Ast.Gt, ">");
  (Ast.NEq, "~=");
  (Ast.And, "&&");
  (Ast.Or, "||");
  (Ast.Cons, "::");
  (Ast.Concat, "..");
]

let unop_m = [
  (Ast.UMinus, "-");
  (Ast.Neg, "!");
]

let rec ast_to_string = function
| Ast.Let(id, e1, e2) -> "let " ^ id ^ " = " ^ ast_to_string  e1 ^ " in " ^ ast_to_string  e2
| Ast.Binop(e1, op, e2) -> "(" ^ ast_to_string e1 ^ " " ^ List.assoc op binop_m ^ " " ^ ast_to_string e2 ^ ")"
| Ast.Unop(op, e) -> "(" ^ List.assoc op unop_m ^ ast_to_string e ^ ")"
| Ast.FunDecl(xs, e) -> "fun " ^ String.concat ", " xs ^ " -> " ^ ast_to_string e
| Ast.Const(CInt n) -> string_of_int n
| Ast.Const(CFloat f) -> string_of_float f
| Ast.Const(CString s) -> "\"" ^ s ^ "\""
| Ast.ListExpr(xs) -> "[" ^ String.concat "\n" (List.map ast_to_string xs) ^ "]"
| Ast.App(e1, e2) -> "(" ^ ast_to_string e1 ^ " . " ^ ast_to_string e2 ^ ")"
| Ast.If(cond, _then, _else) -> "if " ^ ast_to_string cond ^ " then " ^ ast_to_string _then ^ " else " ^ ast_to_string _else
| Ast.Id(id) -> id

let prog_to_str prog = match LangParser.parse prog with
| Some xs -> String.concat "\n" (List.map ast_to_string xs)
| None -> "fail"

let _ = if Array.length Sys.argv = 2 then
  print_endline (prog_to_str (read_file Sys.argv.(1)))
else
  print_endline "Usage: parser_combinators FILE"
