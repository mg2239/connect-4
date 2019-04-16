(**
   Creates a 6x7 grid for a game of Connect 4 that the players can drop colored 
   disks into to make their moves
*)

(**[t] is the type of a board *)
type t

(**[color] represents the colors of a disk. R is a red disk, B is a black disk
   and Emp signifies there is no disk at that position in the board.
*)
type color = R | B | Emp

(**[empty] is the empty board where no moves have been made. It is the initial
   state of the board in a game.
*)
val empty : t

(**[make_move board column color] is the board resulting from dropping a disk
   of color [color] into the column numbered [column] where [column] is a number
   in the range [0..5] counting from the left.
*)
val make_move : t -> int -> color -> t

(**[get_as_list board] is the current state of the board represented as a 
   2 dimensional list of type [color]. The head of a column list is the top disk
   in that list. Each column list has a length of at most 6. 
*)
val get_as_list : t -> (color list) list

(**[check_win board] returns Some [c] if the color [c] has won the game 
   represented by [board]. Returns [None] if no one has won the game
*)
val check_win : t -> color option

(**[filled_slots board] returns the number of slots (positions) in the board
   that have a disk in it. This is equal to the number of moves made since the 
   beginning of the game.
*)
val filled_slots : t -> int

(**[is_full board] returns [true] if all the slots (positions) in [board] have
   disks in them. That is, no more moves can be made
*)
val is_full : t -> bool

(**[is_full_column board c] returns [true] if no more disks can be dropped into 
   column [c]
*)
val is_full_column : t -> int -> bool

(**[ascii_art board] returns a string containing an ASCII art representation
   of [board] that can be printed to the terminal during gameplay
*)
val ascii_art : t -> string


