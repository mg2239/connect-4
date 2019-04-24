(** The type [position] represents a position on the grid with column
    number and the color the player is using. *)
type position = int

(** The type [command] represents a player command that is decomposed
    into a verb, and possibly a position. *)
type command = 
  | Go of position
  | Help
  | Quit

(** Raised when an empty or malformed command is parsed. *)
exception Malformed

let parse str = 
  let str_lst = String.split_on_char ' ' str in
  let check_empty str = str <> "" in 
  let filtered_lst = List.filter check_empty str_lst in 
  match filtered_lst with
  | h::[] -> begin 
      if h = "help" then Help 
      else if h = "quit" then Quit
      else raise Malformed
    end
  | h1::h2::[] -> begin
      let column = begin 
        try int_of_string h2 with
        | Failure t -> 8
      end in 
      if column > 0 && column < 8 && h1 = "go" then Go (column - 1)
      else raise Malformed 
    end 
  | _ -> raise Malformed