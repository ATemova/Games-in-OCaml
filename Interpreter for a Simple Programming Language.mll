(*tokenizer*)
type token = 
  | INT of int
  | PLUS | MINUS | TIMES | DIVIDE
  | LPAREN | RPAREN
  | IF | THEN | ELSE | WHILE | DO
  | EQ | NEQ | LT | GT | LTE | GTE
  | ASSIGN | SEMICOLON | IDENT of string
  | EOF

let is_digit = function '0'..'9' -> true | _ -> false
let is_alpha = function 'a'..'z' | 'A'..'Z' -> true | _ -> false

let rec tokenize input =
  let len = String.length input in
  let rec aux pos tokens =
    if pos >= len then List.rev (EOF :: tokens)
    else match input.[pos] with
      | ' ' | '\t' | '\n' -> aux (pos + 1) tokens
      | '+' -> aux (pos + 1) (PLUS :: tokens)
      | '-' -> aux (pos + 1) (MINUS :: tokens)
      | '*' -> aux (pos + 1) (TIMES :: tokens)
      | '/' -> aux (pos + 1) (DIVIDE :: tokens)
      | '(' -> aux (pos + 1) (LPAREN :: tokens)
      | ')' -> aux (pos + 1) (RPAREN :: tokens)
      | ';' -> aux (pos + 1) (SEMICOLON :: tokens)
      | '=' -> aux (pos + 1) (ASSIGN :: tokens)
      | c when is_digit c ->
        let rec consume_digits i =
          if i < len && is_digit input.[i] then consume_digits (i + 1)
          else i
        in
        let end_pos = consume_digits pos in
        let num = int_of_string (String.sub input pos (end_pos - pos)) in
        aux end_pos (INT num :: tokens)
      | c when is_alpha c ->
        let rec consume_ident i =
          if i < len && (is_alpha input.[i] || is_digit input.[i]) then consume_ident (i + 1)
          else i
        in
        let end_pos = consume_ident pos in
        let ident = String.sub input pos (end_pos - pos) in
        let token = match ident with
          | "if" -> IF | "then" -> THEN | "else" -> ELSE
          | "while" -> WHILE | "do" -> DO
          | _ -> IDENT ident
        in
        aux end_pos (token :: tokens)
      | _ -> failwith "Unexpected character"
  in
  aux 0 []
(*parser*)
type expr =
  | Int of int
  | Var of string
  | BinOp of binop * expr * expr
  | If of expr * expr * expr
  | While of expr * expr
  | Assign of string * expr
  | Seq of expr list
and binop = Add | Sub | Mul | Div | Lt | Gt | Eq | Neq

let rec parse_expr tokens =
  (* A full parser would go here, using recursive descent parsing *)
  (* Placeholder for a complex expression parsing logic *)
  failwith "Not implemented"

let rec parse_stmt tokens =
  match tokens with
  | IDENT x :: ASSIGN :: rest ->
    let expr, rest' = parse_expr rest in
    Assign (x, expr), rest'
  | IF :: rest ->
    let cond, rest1 = parse_expr rest in
    let then_branch, rest2 = parse_stmt rest1 in
    let else_branch, rest3 = parse_stmt (match rest2 with ELSE :: r -> r | _ -> failwith "Syntax error") in
    If (cond, then_branch, else_branch), rest3
  | WHILE :: rest ->
    let cond, rest1 = parse_expr rest in
    let body, rest2 = parse_stmt rest1 in
    While (cond, body), rest2
  | _ -> failwith "Syntax error"
(*interpretere*)
type value = 
  | VInt of int
  | VBool of bool
  | VUnit

let rec eval_expr env = function
  | Int n -> VInt n
  | Var x -> List.assoc x env
  | BinOp (op, e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    eval_binop op v1 v2
  | If (cond, e1, e2) ->
    let VBool b = eval_expr env cond in
    if b then eval_expr env e1 else eval_expr env e2
  | _ -> failwith "Not implemented"

and eval_binop op v1 v2 = match (op, v1, v2) with
  | (Add, VInt n1, VInt n2) -> VInt (n1 + n2)
  | (Sub, VInt n1, VInt n2) -> VInt (n1 - n2)
  | (Mul, VInt n1, VInt n2) -> VInt (n1 * n2)
  | (Div, VInt n1, VInt n2) -> VInt (n1 / n2)
  | _ -> failwith "Type error"

let rec eval_stmt env = function
  | Assign (x, e) -> let v = eval_expr env e in (x, v) :: env
  | Seq stmts -> List.fold_left eval_stmt env stmts
  | If (cond, s1, s2) ->
    let VBool b = eval_expr env cond in
    if b then eval_stmt env s1 else eval_stmt env s2
  | While (cond, body) ->
    let rec loop env =
      let VBool b = eval_expr env cond in
      if b then loop (eval_stmt env body) else env
    in loop env
  | _ -> env
