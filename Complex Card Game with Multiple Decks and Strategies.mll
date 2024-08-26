(* Complex Card Game with Multiple Decks and Strategies *)

type suit = Hearts | Diamonds | Clubs | Spades
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type card = { suit: suit; rank: rank }
type deck = card list
type hand = card list
type game_state = { decks: deck list; hands: hand list; strategies: (string * (card list -> card)) list }

let shuffle_deck (deck: deck) : deck =
  (* Shuffle a deck of cards *)
  deck

let draw_card (deck: deck) : (card * deck) =
  (* Draw a card from the deck *)
  (List.hd deck, List.tl deck)

let apply_strategy (hand: hand) (strategy: string) : card =
  (* Apply a strategy to choose a card from hand *)
  List.hd hand (* Placeholder *)

let play_turn (state: game_state) : game_state =
  (* Play a turn including drawing cards and applying strategies *)
  state
