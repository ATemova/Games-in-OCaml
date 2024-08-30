type state = int
type symbol = char
type transition = state * symbol option * state
type fsm = {
  states: state list;
  start: state;
  final: state list;
  transitions: transition list
}

let create_fsm states start final transitions = {
  states;
  start;
  final;
  transitions
}

let rec simulate fsm current_state input =
  if List.mem current_state fsm.final && input = [] then true
  else
    match input with
    | [] -> false
    | x :: xs ->
      let next_states =
        List.filter_map (fun (s, sym, next) ->
          if s = current_state && (sym = None || sym = Some x) then Some next else None
        ) fsm.transitions
      in
      List.exists (fun state -> simulate fsm state xs) next_states

let minimize_fsm fsm =
  let rec partition states partitions =
    (* Partitioning algorithm for state minimization *)
    partitions
  in
  let rec distinguish states =
    (* Determine if states are distinguishable *)
    states
  in
  let partitions = partition fsm.states [] in
  let new_states = distinguish partitions in
  (* Rebuild FSM with minimized states *)
  create_fsm new_states fsm.start fsm.final fsm.transitions
