open Board

(* The type for a game state *)
type t

(* The type for the result of a move *)
type result = Legal of t | Illegal

(** [init_state] is the initial state of the game *)
val init_state : t

(**  *)
val current_player : t -> string

(**  *)
val next_player : t -> string

(**  *)
val game_state : t -> bool

(**  *)
val go : Board.t -> int -> t -> result