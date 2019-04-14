(* The type for a game state *)
type t

(* The type for the result of a move *)
type result = Legal of t | Illegal

(** [init_state] is the initial state of the game *)
val init_state : t

(** [current_player st] is the current player of the game state *)
val current_player : t -> string

(** [next_player st] is the next player of the game state *)
val next_player : t -> string

(** [game_state st] is whether a player has won or not *)
val game_state : t -> bool

(** [go board column state] *)
val go : Board.t -> int -> t -> result