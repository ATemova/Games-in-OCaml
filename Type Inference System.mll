(*expression and types*)
type typ = 
  | TInt
  | TBool
  | TFun of typ * typ
  | TVar of string
  | TUnit

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr
(*environment and type unification*)
type env = (string * typ) list

let rec unify t1 t2 = match (t1, t2) with
  | (TInt, TInt) | (TBool, TBool) -> []
  | (TFun (a1, b1), TFun (a2, b2)) -> 
      let s1 = unify a1 a2 in
      let s2 = unify (subst_type s1 b1) (subst_type s1 b2) in
      s1 @ s2
  | (TVar x, t) | (t, TVar x) -> if occurs_check x t then failwith "Type error" else [(x, t)]
  | _ -> failwith "Types do not match"

and subst_type s t = match t with
  | TInt | TBool | TUnit -> t
  | TVar x -> (try List.assoc x s with Not_found -> t)
  | TFun (a, b) -> TFun (subst_type s a, subst_type s b)

and occurs_check x t = match t with
  | TVar y when x = y -> true
  | TFun (a, b) -> occurs_check x a || occurs_check x b
  | _ -> false
(*type inference*)
let rec infer env = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x -> (try List.assoc x env with Not_found -> failwith "Unbound variable")
  | Fun (x, e) ->
    let tv = fresh_type_var () in
    let env' = (x, tv) :: env in
    let t = infer env' e in
    TFun (tv, t)
  | App (e1, e2) ->
    let t1 = infer env e1 in
    let t2 = infer env e2 in
    let tv = fresh_type_var () in
    let s = unify t1 (TFun (t2, tv)) in
    subst_type s tv
  | Let (x, e1, e2) ->
    let t1 = infer env e1 in
    let env' = (x, t1) :: env in
    infer env' e2
