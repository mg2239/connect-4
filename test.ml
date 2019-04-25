open OUnit2
open Board
open State
open Command

(** [board_list_print lst] prints out a board in form of a list. *)
let rec board_list_print lst = 
  let rec elt_print e = 
    match e with
    | [] -> "]"
    | h::t -> Board.color_string h ^ ";" ^ elt_print t in
  let rec lst_print l =
    match l with
    | [] -> "]"
    | h::t -> elt_print h ^ ";\n" ^ (board_list_print t) in
  "[" ^ lst_print lst

(** [make_test_board_get_as_list name board expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [get_as_list board] *)
let make_test_board_get_as_list 
    (name: string)
    (board: Board.t)
    (expected_output: (color list) list) : test =
  name >:: (fun _ -> assert_equal 
               expected_output (get_as_list board) ~printer:board_list_print)

(** [make_test_board_check_win name board expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [check_win board]. *)
let make_test_board_check_win
    (name: string)
    (board: Board.t)
    (expected_output: color option) : test =
  name >:: (fun _ -> assert_equal
               expected_output (check_win board))

(**[generate_full_board] is a board with all slots filled with R and B disks*)
let generate_full_board = 
  let emp_board = Board.empty in
  let rec loop count board color= 
    if count = 42 then board
    else 
      let next_color = (
        if color = R then B
        else R
      ) in
      loop (count+1) (Board.make_move board (count mod 7) color) next_color in 
  loop 0 emp_board R

(** [test_full_board name board expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [is_full board]. *)
let test_full_board  
    (name: string)
    (board: Board.t)
    (expected_output: bool) : test = 
  name>:: (fun _ -> assert_equal expected_output (is_full board))

let empty_board = Board.empty
let board1 = Board.make_move empty_board 0 R
let board2 = Board.make_move board1 0 B
let board3 = Board.make_move board1 1 R
let board4 = Board.make_move board3 2 R
let board5 = Board.make_move board4 3 R
let board6 = Board.make_move board1 0 R
let board7 = Board.make_move board6 0 R
let board8 = Board.make_move board7 0 R 
let board9 = Board.make_move board8 1 B
let board10 = Board.make_move board9 1 B
let board11 = Board.make_move board10 1 R
let board12 = Board.make_move board11 2 B
let board13 = Board.make_move board12 2 R
let board14 = Board.make_move board13 3 R
let board_full = generate_full_board


let board_tests = [
  make_test_board_get_as_list "bd_test_empty" empty_board 
    [[Emp;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp];
     [Emp;Emp;Emp;Emp;Emp;Emp];
     [Emp;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp];
     [Emp;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp]];
  make_test_board_get_as_list "bd_test_makemove1" board1
    [[R;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp];
     [Emp;Emp;Emp;Emp;Emp;Emp];
     [Emp;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp];
     [Emp;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp]];
  make_test_board_get_as_list "bd_test_makemove2" board2
    [[R;B;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp];
     [Emp;Emp;Emp;Emp;Emp;Emp];
     [Emp;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp];
     [Emp;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp]];
  make_test_board_check_win "bd_test_check_win1" board5
    (Some R);
  make_test_board_check_win "bd_test_check_win2" board1 None;
  make_test_board_check_win "bd_test_check_win3" board8 (Some R);
  make_test_board_check_win "bd_test_check_win4" board14 (Some R);
  make_test_board_check_win "bd_test_check_win2" board2 None;
  make_test_board_check_win "bd_test_check_win2" board3 None;
  make_test_board_check_win "bd_test_check_win2" board4 None;
  test_full_board "not full board 0" empty_board (false);
  test_full_board "not full board 1" board1 (false);
  test_full_board "not full board 2" board2 (false);
  test_full_board "not full board 3" board3 (false);
  test_full_board "not full board 4" board4 (false);
  test_full_board "not full board 5" board5 (false);
  test_full_board "not full board 6" board6 (false);
  test_full_board "full board" board_full (true);
]

(** [result_match] returns a state given an option result. *)
let result_match = function
  | Legal c -> c
  | Illegal -> State.init_state

let init_st = State.init_state
let st2_result = State.go 0 init_st
let st2 = result_match st2_result
let st3_result = State.go 1 st2
let st3 = result_match st3_result

(**CHECK THIS OUT IMAAN WE NEED A TESTS FOR A FULL BOARD *)
let board_function_tests = [
  "board test_is_full 1" >:: (fun _ ->
      assert_equal (Board.is_full empty_board) false); 
  "board test_is_full 2" >:: (fun _ ->
      assert_equal (Board.is_full board1) false);
  "board test_is_full 2" >:: (fun _ ->
      assert_equal (Board.is_full board2) false);
]


let state_tests = [
  "board test 1" >:: (fun _ ->
      assert_equal Board.empty (State.board init_st));
  "board test 2" >:: (fun _ ->
      assert_equal [[R;Emp;Emp;Emp;Emp;Emp]; 
                    [B;Emp;Emp;Emp;Emp;Emp]; 
                    [Emp;Emp;Emp;Emp;Emp;Emp]; 
                    [Emp;Emp;Emp;Emp;Emp;Emp]; 
                    [Emp;Emp;Emp;Emp;Emp;Emp]; 
                    [Emp;Emp;Emp;Emp;Emp;Emp]; 
                    [Emp;Emp;Emp;Emp;Emp;Emp]]
        (Board.get_as_list (State.board st3)) ~printer:board_list_print);
  "current test 1" >:: (fun _ ->
      assert_equal R (State.current_player init_st));
  "current test 2" >:: (fun _ ->
      assert_equal B (State.current_player st2));
  "next test 1" >:: (fun _ ->
      assert_equal R (State.next_player st2));
  "next test 2" >:: (fun _ ->
      assert_equal B (State.next_player init_st));
]


(** [make_test_command name str expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [parse str] *)
let make_test_command
    (name : string)
    (str: string)
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse str)) 

(** [make_test_command_malformed name str] constructs an OUnit
    test named [name] that asserts that an [Malformed] exception is raised
    with [parse str]. *)
let make_test_command_malformed
    (name : string) 
    (str: string) : test = 
  name >:: (fun _ -> 
      assert_raises (Malformed) (fun () -> (parse str)))

let command_tests =
  [
    make_test_command "cmd_test1" "1" (Go 0);
    make_test_command "cmd_test2" "        2" (Go 1);
    make_test_command "cmd_test3" "        7  " (Go 6);
    make_test_command "cmd_test14" "help" Help;
    make_test_command_malformed "cmd_test4" "go -1 ";
    make_test_command_malformed "cmd_test5" "oh";
    make_test_command_malformed "cmd_test6" "";
    make_test_command_malformed "cmd_test7" "       ";
    make_test_command_malformed "cmd_test8" "go   ";
    make_test_command_malformed "cmd_test9" "GO";
    make_test_command_malformed "cmd_test10" "11";
    make_test_command_malformed "cmd_test11" "help me";
    make_test_command_malformed "cmd_test12" "go 7 R";
    make_test_command_malformed "cmd_test13" "B";
  ]

let suite =
  "test suite for A6"  >::: List.flatten [
    board_tests;
    state_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite