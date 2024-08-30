type 'a parser = char list -> ('a * char list) option

let return x = fun input -> Some (x, input)

let (>>=) p f = fun input ->
  match p input with
  | None -> None
  | Some (x, input') -> f x input'

let (>>|) p f = p >>= fun x -> return (f x)

let char c = function
  | [] -> None
  | x :: xs when x = c -> Some (c, xs)
  | _ -> None

let rec many p input = match p input with
  | None -> Some ([], input)
  | Some (x, input') ->
    (match many p input' with
     | Some (xs, input'') -> Some (x :: xs, input'')
     | None -> Some ([x], input'))

let choice ps input = 
  let rec try_parse = function
    | [] -> None
    | p :: ps' -> (match p input with None -> try_parse ps' | res -> res)
  in try_parse ps

type position = { line: int; col: int }
type 'a parser_result = ('a * char list * position) option

let update_position pos = function
  | '\n' -> { line = pos.line + 1; col = 1 }
  | _ -> { pos with col = pos.col + 1 }

let satisfy f = fun (input, pos) ->
  match input with
  | [] -> None
  | c :: cs when f c -> Some (c, cs, update_position pos c)
  | _ -> None

let parse_with_position p input =
  let rec aux pos input =
    match input with
    | [] -> None
    | _ -> p (input, pos)
  in aux { line = 1; col = 1 } input

module Memo = Map.Make(struct type t = int let compare = compare end)

let memoize p =
  let memo_table = ref Memo.empty in
  fun input ->
    let key = List.length input in
    match Memo.find_opt key !memo_table with
    | Some result -> result
    | None ->
      let result = p input in
      memo_table := Memo.add key result !memo_table;
      result

let rec parse_expression = memoize (fun input ->
  parse_with_position (choice [parse_term; parse_factor; parse_literal]) input)

(* Where parse_term, parse_factor, and parse_literal are defined *)
