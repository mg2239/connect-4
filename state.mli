type t

type result = Legal of t | Illegal

val init_state : t

val next_player : t -> string

val game_state : t -> bool

val go : Board.t -> int -> string -> result