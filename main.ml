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

let () = main ()