type t = {
  board: Board.t;
  current: Board.color
}

type result = Legal of t | Illegal

let init_state = {
  board = Board.empty;
  current = R
}

let board st = 
  st.board

let current_player st =
  st.current

let next_player st = 
  if st.current = Board.R then Board.B else Board.R

let game_state st = 
  match Board.check_win st.board with
  | Some c -> false
  | None -> true

let go col st = 
  let b = st.board in
  if not (Board.is_full_column b col) then Legal ({
      board = (Board.make_move b col st.current);
      current = next_player st
    })
  else Illegal