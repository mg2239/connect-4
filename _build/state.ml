open Board

type t = {
  board: Board.t;
  current: color
}

type result = Legal of t | Illegal

let init_state = {
  board = Board.empty;
  current = R
}

let current_player st =
  st.current

let next_player st = 
  if st.current = R then B else R

let game_state st = 
  match check_win st.board with
  | Some c -> false
  | None -> true

let go b col st = 
  if not (is_full_column b col) then Legal ({
      board = (make_move b col st.current);
      current = next_player st
    })
  else Illegal