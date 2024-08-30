type term = 
  | Var of string
  | Atom of string
  | Compound of string * term list

type fact = Fact of term
type rule = Rule of term * term list

type program = {
  facts: fact list;
  rules: rule list
}

let rec unify t1 t2 subst = match (t1, t2) with
  | (Var v, _) -> if List.mem_assoc v subst then unify (List.assoc v subst) t2 subst else (v, t2) :: subst
  | (_, Var v) -> if List.mem_assoc v subst then unify t1 (List.assoc v subst) subst else (v, t1) :: subst
  | (Atom a1, Atom a2) when a1 = a2 -> subst
  | (Compound (f1, args1), Compound (f2, args2)) when f1 = f2 ->
    List.fold_left2 (fun subst (a1, a2) -> unify a1 a2 subst) subst (List.combine args1 args2)
  | _ -> failwith "Unification failed"

let rec resolve_query program query subst =
  let try_fact (Fact f) =
    try Some (unify query f subst) with _ -> None
  in
  let try_rule (Rule (head, body)) =
    let subst = unify query head subst in
    List.fold_left (fun subst q -> resolve_query program q subst) subst body
  in
  match List.find_map try_fact program.facts with
  | Some subst -> Some subst
  | None -> List.find_map (try_rule) program.rules

let rec interpret program query =
  match resolve_query program query [] with
  | Some subst -> print_endline ("Success: " ^ (string_of_subst subst))
  | None -> print_endline "Failed to resolve query"
