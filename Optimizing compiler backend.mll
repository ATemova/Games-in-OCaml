type ir =
  | Const of int
  | Add of ir * ir
  | Mul of ir * ir
  | Var of string
  | Assign of string * ir
  | Seq of ir list
  | If of ir * ir * ir

let rec fold_constants = function
  | Add (Const x, Const y) -> Const (x + y)
  | Mul (Const x, Const y) -> Const (x * y)
  | Add (x, y) -> Add (fold_constants x, fold_constants y)
  | Mul (x, y) -> Mul (fold_constants x, fold_constants y)
  | If (cond, then_, else_) ->
    If (fold_constants cond, fold_constants then_, fold_constants else_)
  | Assign (v, expr) -> Assign (v, fold_constants expr)
  | Seq exprs -> Seq (List.map fold_constants exprs)
  | other -> other

let rec eliminate_dead_code live_vars = function
  | Assign (v, _) as assign -> if List.mem v live_vars then [assign] else []
  | Seq stmts ->
    let rec elim = function
      | [] -> []
      | stmt :: rest ->
        let live_rest = eliminate_dead_code live_vars rest in
        (match stmt with
         | Assign (v, expr) ->
           if List.exists (fun s -> List.mem v (live_vars_of_stmt s)) live_rest then
             stmt :: live_rest
           else
             eliminate_dead_code live_vars rest
         | _ -> stmt :: live_rest)
    in elim stmts
  | other -> [other]

and live_vars_of_stmt = function
  | Assign (_, expr) -> free_vars expr
  | _ -> []

let rec generate_code = function
  | Const x -> Printf.sprintf "PUSH %d" x
  | Add (x, y) ->
    let code_x = generate_code x in
    let code_y = generate_code y in
    Printf.sprintf "%s\n%s\nADD" code_x code_y
  | Mul (x, y) ->
    let code_x = generate_code x in
    let code_y = generate_code
