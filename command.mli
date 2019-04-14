(** The type [position] represents a position on the grid with column
    number and the color the player is using. *)
type position = int * string


(** The type [command] represents a player command that is decomposed
    into a verb, and possibly a position. *)
type command = 
  | Go of position
  | Help


(** Raised when an malformed command is parsed. *)
exception Malformed


(** [parse str] parses a player's input into a [command]. The first
    word (consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest, if any, becomes the position.
    Examples: 
    - [parse "    go   6   black  "] is [Go (6, "black")]
    - [parse "help"] is [Help]
      Requires: [str] contains only a-z, 0-9, and space 
      characters (only ASCII character code 32; not tabs or newlines, etc.).
      @raise Malformed if the command is invalid. A command is malformed 
      if the verb is neither "go" nor "help",
      or if [str] is the empty string or contains only spaces, 
      or if the verb is "help" and there is a non-empty position,
      or if the verb is "go" and the position doesn't contain a valid position.
      @param str - the player's input
*)
val parse : string -> command