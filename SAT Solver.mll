(*CNF representation*)
type literal = int
type clause = literal list
type cnf = clause list
(*unit propagation*)
let rec unit_propagate (clauses: cnf) (assignment: literal list) =
  match clauses with
  | [] -> Some assignment
  | clause :: rest ->
    match clause with
    | [l] -> 
      let updated_clauses = List.map (List.filter ((<>) (-l))) rest in
      unit_propagate updated_clauses (l :: assignment)
    | _ -> 
      let updated_clauses = List.filter ((<>) (-List.hd clause)) clauses in
      unit_propagate updated_clauses assignment
(*pure literal elimination*)
let rec pure_literal_elimination (clauses: cnf) (assignment: literal list) =
  let literals = List.flatten clauses in
  let rec find_pure ls = match ls with
    | [] -> None
    | l :: rest -> if List.mem (-l) literals then find_pure rest else Some l
  in
  match find_pure literals with
  | Some l ->
    let updated_clauses = List.filter (fun clause -> not (List.mem l clause)) clauses in
    pure_literal_elimination updated_clauses (l :: assignment)
  | None -> (clauses, assignment)
(*DPLL algorithm*)
let rec dpll (clauses: cnf) (assignment: literal list) =
  let clauses, assignment = pure_literal_elimination clauses assignment in
  match unit_propagate clauses assignment with
  | Some a -> Some a
  | None -> 
    if clauses = [] then Some assignment
    else 
      let l = List.hd (List.hd clauses) in
      let result = dpll (List.map (List.filter ((<>) (-l))) clauses) (l :: assignment) in
      match result with
      | Some _ -> result
      | None -> dpll (List.map (List.filter ((<>) l)) clauses) ((-l) :: assignment)
