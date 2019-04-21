(** [game_1 state] takes in user input and reads, evaluates, 
    prints and loops for the Connect Four game. Depending on the user input, 
    it will update the game state accordingly.
    @param state is of type State.t and represents the current game state.
*)
let rec game_1 state = failwith "" 

(** [game_2 state] takes in user input and reads, evaluates, 
    prints and loops for the Connect Four game. Depending on the user input, 
    it will update the game state accordingly.
    @param state is of type State.t and represents the current game state.
*)
let rec game_2 state = 
  print_string ("\n" ^ Board.ascii_art (State.board state));
  print_int (Board.score (State.board state));
  match Board.check_win (State.board state) with 
  | Some c -> print_string ((Board.color_string c) ^ " wins!\n"); exit 0
  | None -> begin
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
      | Command.Help -> begin 
          print_string ("\nWelcome to the 3110 Connect Four Game!" ^
                        "\nIn Connect Four, the object of the game is to get " ^
                        "four of your game pieces in a row either vertically, " ^
                        "horizontally, or diagonally." ^
                        "\nYou can also place pieces in a fashion to block your " ^ 
                        "opponent from getting four in a row." ^
                        "\nTo place a game piece, type: \"go (1-7)\" where the " ^
                        "number you input represents the respective column in " ^
                        "the game board." ^
                        "\nType \"quit\" to quit the game." ^
                        "\nIf you need to see this message again, type " ^ 
                        "\"help\" into the console.\n\n");
          game_2 state;
        end
      | Command.Quit -> begin 
          print_string "Thanks for playing!\n"; 
          exit 0
        end
      | exception Command.Malformed -> begin
          print_string ("Your command was malformed. "^ 
                        "Please submit a valid command \n");
          game_2 state;
        end
    end

let rec start_game () = 
  print_string ("Choose game mode: 1 Player | 2 Player\n");
  print_string ("> ");
  match read_line() with
  | "1" | "1 Player" -> game_1 (State.init_state)
  | "2" | "2 Player" -> game_2 (State.init_state)
  | _ -> print_string "\nInvalid game mode, try again.\n\n"; start_game ()

(* [main] prompts users with Connect four game instructions and starts the game.
   @param () is of type unit. **)
let main () = 
  print_string ("\nWelcome to the 3110 Connect Four Game!"^
                "\nIn Connect Four, the object of the game is to get four "^ 
                "of your game pieces in a row either vertically, horizontally, "^ 
                "or diagonally." ^
                "\nYou can also place pieces in a fashion to block your "^ 
                "opponent from getting four in a row." ^
                "\nTo place a game piece, type: \"go (1-7)\" where the number "^ 
                "you input represents the respective column in the game board."^
                "\nType \"quit\" to quit the game." ^
                "\nIf you need to see this message again, type "^ 
                "\"help\" into the console.\n\n");
  start_game ()

(* [()] executes the main.byte file which runs the game**)
let () = main ()