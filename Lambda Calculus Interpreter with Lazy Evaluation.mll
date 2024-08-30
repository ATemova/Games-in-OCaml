(*expressions*)
type expr =
  | Var of string
  | Lam of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | LetRec of string * expr * expr
(*substitution*)
let rec subst v e body = match body with
  | Var x when x = v -> e
  | Var x -> Var x
  | Lam (x, body') when x <> v -> Lam (x, subst v e body')
  | App (e1, e2) -> App (subst v e e1, subst v e e2)
  | Let (x, e1, e2) when x <> v -> Let (x, subst v e e1, subst v e e2)
  | LetRec (x, e1, e2) when x <> v -> LetRec (x, subst v e e1, subst v e e2)
  | _ -> body
(*lazy evaluation*)
let rec eval env expr = match expr with
  | Var x -> (try List.assoc x env with Not_found -> expr)
  | Lam _ -> expr
  | App (e1, e2) ->
    let f = eval env e1 in
    (match f with
     | Lam (x, body) -> eval ((x, e2) :: env) body
     | _ -> App (f, e2))
  | Let (x, e1, e2) ->
    let v1 = eval env e1 in
    eval ((x, v1) :: env) e2
  | LetRec (f, Lam (x, body), e2) ->
    let rec_env = ref [] in
    let v = Lam (x, LetRec (f, Lam (x, body), body)) in
    rec_env := (f, v) :: env;
    eval !rec_env e2
  | _ -> expr
