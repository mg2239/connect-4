open Board

type t = {
  board: Board.t;
  current: string
}

type result = Legal of t | Illegal

let init_state = {
  board = Board.empty;
  current = "R"
}

let next_player st = 
  if st.current = "R" then {st with current = "B"} else {st with current = "R"}

let game_state st = 
  match check_win st.board with
  | Some c -> true
  | None -> false

let go board column color = 
  if is_full_col column then Legal ({
      board: Board.make_move board column color;
      current: color
    })
  else Illegal