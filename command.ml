type position = int * string

type command = 
  | Go of position
  | Help

exception Malformed

let parse str = 
  if (str = "") || (String.trim str = "") then raise Malformed
  else let str_lst = String.split_on_char ' ' str in 
    let check_empty str = str <> "" in 
    let filtered_lst = List.filter check_empty str_lst in 
    let verb = (List.hd filtered_lst) in 
    if not (List.mem verb ["go"; "help"])
    || (verb = "help" && (List.length filtered_lst > 1))
    || (verb = "go" && (List.length filtered_lst <> 3))
    then raise Malformed
    else if verb = "help" then Help
    else let col = int_of_string_opt (List.nth filtered_lst 1) in
      let color = int_of_string_opt (List.nth filtered_lst 2) in
      match col, color with
      | (Some i, None) -> Go (i, List.nth filtered_lst 2)
      | _ -> raise Malformed
