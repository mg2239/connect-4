let rec game state = 
  print_string (Board.ascii_art (State.board state));
  let color = if State.current_player state = R then "Red" else "Black" in
  print_string (color ^ "'s turn: ");
  match (Command.parse (read_line ())) with
  | Command.Go col -> begin 
      let result =(State.go (State.board state) col state) in
      match result with
      | Legal updated_state -> begin 
          if State.game_state updated_state then game updated_state
          else print_string (color ^ " wins!"); exit 0
        end
      | Illegal -> print_string "Your move was invalid, please try again.";
        game state;
    end
  | Command.Help -> begin 
      print_string ("\nIn Connect Four, the object of the game is to get four " ^ 
                    "of your game pieces in a row either vertically, horizontally," ^ 
                    "or diagonally." ^
                    "\nYou can also place pieces in a fashion to block your " ^ 
                    "opponent from getting four in a row." ^
                    "\nTo place a game piece, type: \"go (0-6)\" where the number " ^ 
                    "you input represents the respective column in the game board." ^
                    "\nIf you need to see this message again, type " ^ 
                    "\"help\" into the console.\n\n");
      game state;
    end
  | Command.Quit -> print_string "Thanks for playing!\n"; exit 0
  | exception Command.Malformed -> begin
      print_string "Your command was malformed. Please submit a valid command \n";
      game state;
    end

(* let initialize_game = to be included in future iterations for more advanced functionality **)

let main () = 
  print_string ("\nWelcome to the 3110 Connect Four Game!" ^
                "\nIn Connect Four, the object of the game is to get four " ^ 
                "of your game pieces in a row either vertically, horizontally," ^ 
                "or diagonally." ^
                "\nYou can also place pieces in a fashion to block your " ^ 
                "opponent from getting four in a row." ^
                "\nTo place a game piece, type: \"go (0-6)\" where the number " ^ 
                "you input represents the respective column in the game board." ^
                "\nIf you need to see this message again, type " ^ 
                "\"help\" into the console.\n\n");
  game (State.init_state)

let () = main ()