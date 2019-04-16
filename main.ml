
(** [game state] takes in user input and reads, evaluates, 
    prints and loops for the Connect Four game. Depending on the user input, 
    it will update the game state accordingly.
    @param state is of type State.t and represents the current game state.
*)
let rec game state = 
  print_string ("\n" ^ (Board.ascii_art (State.board state)));
  let color = if State.current_player state = R then "Red" else "Black" in
  print_string (color ^ "'s turn: ");
  match (Command.parse (read_line ())) with
  | Command.Go col -> begin 
      let result =(State.go col state) in
      match result with
      | Legal updated_state -> begin 
          if State.game_state updated_state then game updated_state
          else print_string (color ^ " wins!\n"); exit 0
        end
      | Illegal -> begin
          print_string "Your move was invalid, please try again.\n";
          game state;
        end
    end
  | Command.Help -> begin 
      print_string ("\nWelcome to the 3110 Connect Four Game!" ^
                    "\nIn Connect Four, the object of the game is to get " ^
                    "four of your game pieces in a row either vertically, " ^
                    "horizontally, or diagonally." ^
                    "\nYou can also place pieces in a fashion to block your " ^ 
                    "opponent from getting four in a row." ^
                    "\nTo place a game piece, type: \"go (0-6)\" where the " ^
                    "number you input represents the respective column in " ^
                    "the game board." ^
                    "\nIf you need to see this message again, type " ^ 
                    "\"help\" into the console.\n\n");
      game state;
    end
  | Command.Quit -> begin 
      print_string "Thanks for playing!\n"; 
      exit 0
    end
  | exception Command.Malformed -> begin
      print_string ("Your command was malformed. "^ 
                    "Please submit a valid command \n");
      game state;
    end

(* [main] prompts users with Connect four game instructions and starts the game.
   @param () is of type unit. **)
let main () = 
  print_string ("\nWelcome to the 3110 Connect Four Game!"^
                "\nIn Connect Four, the object of the game is to get four "^ 
                "of your game pieces in a row either vertically, horizontally,"^ 
                "or diagonally." ^
                "\nYou can also place pieces in a fashion to block your "^ 
                "opponent from getting four in a row." ^
                "\nTo place a game piece, type: \"go (0-6)\" where the number "^ 
                "you input represents the respective column in the game board."^
                "\nIf you need to see this message again, type "^ 
                "\"help\" into the console.\n\n");
  game (State.init_state)

(* [()] executes the main.byte file which runs the game**)
let () = main ()