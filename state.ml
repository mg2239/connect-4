type t = {
  board: Board.t;
  current: string
}

type result = Legal of t | Illegal

let init_state = {
  board = Board.empty;
  current = "R"
}

let current_player st =
  st.current

let next_player st = 
  if st.current = "R" then "B" else "R"

let game_state st = 
  match check_win st.board with
  | Some c -> true
  | None -> false

let go board column state = 
  if not (is_full_col column) then Legal ({
      board: Board.make_move board column (current_player state);
      current: next_player state
    })
  else Illegal