type position = int

type command = 
  | Go of position
  | Help

exception Malformed

let parse str = 
  let str_lst = String.split_on_char ' ' str in
  let check_empty str = str <> "" in 
  let filtered_lst = List.filter check_empty str_lst in 
  match filtered_lst with
  | h::[] -> if h = "help" then Help else raise Malformed
  | h1::h2::[] -> begin
      let column = begin 
        try int_of_string h2 with
        | Failure t -> 7
      end in 
      if column < 7 then Go (column)
      else raise Malformed 
    end 
  | _ -> raise Malformed