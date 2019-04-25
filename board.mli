(**
   Creates a 6x7 grid for a game of Connect 4 that the players can drop colored 
   disks into to make their moves.
*)

(** [color] represents the colors of a disk. R is a red disk, B is a black disk
    and Emp signifies there is no disk at that position in the board. *)
type color = R | B | Emp

(** [t] is the type of a board *)
type t

(** [empty] is the empty board where no moves have been made. It is the initial
    state of the board in a game. *)
val empty : t

(** [ascii_art board] is a string containing an ASCII art representation
    of [board] that can be printed to the terminal during gameplay
    Example: [ascii_art Board.empty] is
             ["O  O  O  O  O  O  O  \n
               O  O  O  O  O  O  O  \n
               O  O  O  O  O  O  O  \n
               O  O  O  O  O  O  O  \n
               O  O  O  O  O  O  O  \n
               O  O  O  O  O  O  O  \n"]
    Raises: None.
    @param board The game board.
    @return [board] as ASCII art. *)
val ascii_art : t -> string

(** [make_move board column color] is the board resulting from dropping a disk
    of color [color] into the column numbered [column] where [column] is a number
    in the range [0..6] counting from the left. 
    Example: [make_move empty 0 R] is [b] where [b] has a red disk in column 0.
    @raise Failure if [column] is greater than 6.
    @param board The game board.
    @param column A column on the board.
    @param color The color of the disk.
    @return [b] where [b] has a disk of color [color] in column [column]. *)
val make_move : t -> int -> color -> t

(** [get_as_list board] is the current state of the board represented as a 
    2 dimensional list of type [color]. The head of a column list is the top disk
    in that list. Each column list has a length of at most 6. 
    Example: [get_as_list Board.empty] is 
             [[[Emp;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp];
              [Emp;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp];
              [Emp;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp];
              [Emp;Emp;Emp;Emp;Emp;Emp]]]
    Raises: None.
    @param board The game board.
    @return [board] as a two-dimensional list. *)
val get_as_list : t -> (color list) list

(** [check_win board] is [Some c] if the color [c] has won the game 
    represented by [board], or [None] if no one has won the game. 
    Example: [check_win b] where in [b] the player of color [B] has a four in a
             row is [Some B].
    Raises: None.
    @param board The game board.
    @return [Some c] is [c] has won, otherwise [None]. *)
val check_win : t -> color option

(** [filled_slots board] is the number of slots (positions) in the board
    that have a disk in it. This is equal to the number of moves made since the 
    beginning of the game. 
    Example: [filled_slots Board.empty] is [0].
    Raises: None.
    @param board The game board.
    @return The number of slots that are filled in [board]. *)
val filled_slots : t -> int

(** [is_full board] is [true] if all the slots (positions) in [board] have
    disks in them. That is, no more moves can be made.
    Example: [is_full Board.empty] is [false]
    Raises: None.
    @param board The game board.
    @return true if [board] contains no [Emp], false otherwise. *)
val is_full : t -> bool

(** [is_full_column board c] is [true] if no more disks can be dropped into 
    column [c]
    Example: [is_full_column Board.empty 0] is [false].
    Raises: None.
    @param board The game board.
    @param c The column number
    @return true if [c] contains no [Emp], false otherwise. *)
val is_full_column : t -> int -> bool

(** [color_string color] is the color in string format.
    Example: [color_string R] is ["Red"]
    Raises: None.
    @param color The color of a disk.
    @return The color as a string. *)
val color_string : color -> string

(** [score t] is the value that represents whether the player
    or the AI is currently winning in the current game state [t].
    Example: [score t] is ["-1"]
    Raises: None.
    @param t The current game state.
    @return a positive int if the AI is winning or a negative value
    if the player is winning. *)
val score : t -> int