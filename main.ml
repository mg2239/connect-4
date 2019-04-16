<<<<<<< HEAD
let main () =
  ANSITerminal.(print_string [red]
                  "\nWelcome to Connect Four!\n");
  print_string ("Type \"1 player\" to play against the computer, or" ^ 
                " \"2 player\" to play against a friend. \n");
  print_string  "> ";
  match read_line () with
  | "1 player" -> failwith "1"
  | "2 player" -> failwith "2"
  | _ -> failwith "invalid"

=======
open State
open Command
open Board

let rec game state = print_string (Board.ascii_art State.board);
    match (let input = (Command.parse (read_line())) in input) with
    | Command.Go col -> begin 
        let result =(State.go (State.board state) col state) in
        match result with
        | Legal updated_state -> game updated_state (* need to process game win/lost state**)
        | Illegal -> print_string "Your move was invalid, please try again.";
                    game state;
    end
    | Command.Help -> begin 
        (print_string ("In Connect Four, the object of the game is "^
    "to get four game pieces in a row eiter vertically, "^
    "horizontally, or diagonally. You can also place pieces in a fashion "^
    "to block your opponent from getting four in a row. To place a game piece, type: "^ 
    "\"Go <insert integer from 0-6 here>\" where the number you input represents the "^
    "respective column in the game board. If you need to see this message again, type "^
    "\"Help\" into the console."));
        game state;
    end
    | exception Command.Malformed -> begin
        print_string "Your command was malformed. Please submit a valid command \n";
        game state;
    end


(* let initialize_game = to be included in future iterations for more advanced functionality **)

let main () = 
    (print_string ("\n\nWelcome to the 3110 Connect Four Game!" ^
    "In Connect Four, the object of the game is to get four game pieces in a row eiter vertically, "^
    "horizontally, or diagonally. You can also place pieces in a fashion "^
    "to block your opponent from getting four in a row. To place a game piece, type: "^ 
    "\"Go <insert integer from 0-6 here>\" where the number you input represents the "^
    "respective column in the game board. If you need to see this message again, type "^
    "\"Help\" into the console."));
    game (State.init_state);

>>>>>>> 30c1056148cc07ca699c89848d39b3ed42c56497
let () = main ()