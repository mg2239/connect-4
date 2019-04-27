open Board

(** The type for a game state. *)
type t = {
  board: Board.t;
  current: color
}

(** The type for the result of a move. *)
type result = Legal of t | Illegal

(** [init_state] is the initial state of the game. *)
let init_state = {
  board = Board.empty;
  current = R
}

(** [board st] is the board of the game state [st]. *)
let board st = 
  st.board

(** [current_player st] is the current player of the game state [st]. *)
let current_player st =
  st.current

(** [next_player st] is the next player of the game state [st]. *)
let next_player st = 
  if st.current = R then B else R

(** [game_state st] is whether a player has gotten four in a row in game
    state [st]. *)
let game_state st = 
  match Board.check_win st.board with
  | Some c -> false
  | None -> true

(** [go col st] is [r] if attempting to place a disk in column [col] of the 
    board in state [st] results in [r]. *)
let go col st = 
  let b = st.board in
  if not (is_full_column b col) then Legal ({
      board = (make_move b col st.current);
      current = next_player st
    })
  else Illegal