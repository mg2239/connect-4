open Aiplayer
open Board

(** The message describing how to play the Connect Four game. *)
let help_msg = 
  "\nIn Connect Four, the object of the game is to get " ^
  "four of your game pieces in a row either vertically, " ^
  "horizontally, or diagonally." ^
  "\nYou can also place pieces in a fashion to block your " ^ 
  "opponent from getting four in a row." ^
  "\nTo place a game piece, type a number \"1-7\" where the " ^
  "number you input represents the respective column in " ^
  "the game board." ^
  "\nType \"quit\" to quit the game." ^
  "\nIf you need to see this message again, type " ^ 
  "\"help\" into the console.\n"

(** [game_1 state] takes in user input and reads, evaluates, 
    prints and loops the Connect Four game. Depending on the user input, 
    it will update the game state accordingly.
    @param state The current game state.
    @param depth The depth of the AI's minimax tree. *)
let rec game_1 state depth = 
  print_string ("\n" ^ Board.ascii_art (State.board state));
  let b = State.board state in
  match (Board.check_win b, Board.is_full b) with 
  | (Some c, _) -> print_string ((Board.color_string c) ^ " wins!\n"); exit 0
  | (None, true) -> print_string "It's a tie!\n"; exit 0
  | _ -> begin
      let color = Board.color_string (State.current_player state) in
      match color with
      | "Red" -> begin
          print_string (color ^ "'s turn: ");
          match (Command.parse (read_line ())) with
          | Command.Go col -> begin 
              let result = (State.go col state) in
              match result with
              | Legal updated_state -> game_1 updated_state depth
              | Illegal -> begin
                  print_string "Your move was invalid, please try again.\n";
                  game_1 state depth;
                end
            end
          | Command.Help -> print_string help_msg; game_1 state depth;
          | Command.Quit -> print_string "Thanks for playing!\n"; exit 0
          | exception Command.Malformed -> begin
              print_string ("Your command was malformed. "^ 
                            "Please submit a valid command \n");
              game_1 state depth;
            end
        end
      | "Blue" -> begin
          print_string "AI is thinking...\n";
          let result = Aiplayer.make_move_ai state depth in
          match result with
          | Legal updated_state -> game_1 updated_state depth
          | Illegal -> 
            print_string "Oops AI move was invalid...\n";
            game_1 state depth;
        end
      | _ -> print_string "Your input was invalid, please try again.\n";
        game_1 state depth;
    end

(** [game_2 state] takes in user input and reads, evaluates, 
    prints and loops the Connect Four game. Depending on the user input, 
    it will update the game state accordingly.
    @param state The current game state. *)
let rec game_2 state = 
  print_string ("\n" ^ Board.ascii_art (State.board state));
  let b = State.board state in
  match (Board.check_win b, Board.is_full b) with 
  | (Some c, _) -> print_string ((Board.color_string c) ^ " wins!\n"); exit 0
  | (None, true) -> print_string "It's a tie!\n"; exit 0
  | _ -> begin
      let color = Board.color_string (State.current_player state) in
      print_string (color ^ "'s turn: ");
      match (Command.parse (read_line ())) with
      | Command.Go col -> begin 
          let result = (State.go col state) in
          match result with
          | Legal updated_state -> game_2 updated_state
          | Illegal -> begin
              print_string "Your move was invalid, please try again.\n";
              game_2 state;
            end
        end
      | Command.Help -> print_string help_msg; game_2 state;
      | Command.Quit -> print_string "Thanks for playing!\n"; exit 0
      | exception Command.Malformed -> begin
          print_string ("Your command was malformed. "^ 
                        "Please submit a valid command \n");
          game_2 state;
        end
    end

(** [choose_difficulty ()] prompts user to choose the difficulty of the
    1 player game. *)
let rec choose_difficulty () = 
  print_string "\nChoose difficulty: Easy (1) | Medium (2) | Hard (3) | Extreme (4)\n";
  print_string "> ";
  match read_line() with
  | "Easy" | "E" | "1" -> game_1 (State.init_state) 2
  | "Medium" | "M" | "2" -> game_1 (State.init_state) 3
  | "Hard" | "H" | "3" -> game_1 (State.init_state) 4
  | "Extreme" | "4" -> game_1 (State.init_state) 5
  | _ -> print_string "\nInvalid difficulty, try again.\n"; choose_difficulty ()

(** [start_game ()] prompts user to choose to play a 1 or 2 player game. *)
let rec start_game () = 
  print_string "\nChoose game mode: 1 Player | 2 Player\n";
  print_string "> ";
  match String.lowercase_ascii (read_line()) with
  | "1" | "1 player" -> choose_difficulty ()
  | "2" | "2 player" -> game_2 (State.init_state)
  | _ -> print_string "\nInvalid game mode, try again.\n"; start_game ()

(** [main] prompts users with game instructions and starts the game. *)
let main () = 
  print_string ("\nWelcome to the 3110 Connect Four Game!\n" ^ help_msg); 
  start_game ()

(** Executes the main.byte file which runs the game. *)
let () = main ()