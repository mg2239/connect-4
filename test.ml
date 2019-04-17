open OUnit2
open Board
open State
open Command

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


let empty_board = Board.empty
let board1 = Board.make_move empty_board 0 R
let board2 = Board.make_move board1 0 B

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
    [[B;R;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp];
     [Emp;Emp;Emp;Emp;Emp;Emp];
     [Emp;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp];
     [Emp;Emp;Emp;Emp;Emp;Emp];[Emp;Emp;Emp;Emp;Emp;Emp]];
]


let result_match = function
  | Legal c -> c
  | Illegal -> State.init_state

let init_st = State.init_state
let st2_result = State.go 0 init_st
let st2 = result_match st2_result
let st3_result = State.go 1 st2
let st3 = result_match st3_result

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
    make_test_command "cmd_test1" "go 1" (Go 0);
    make_test_command "cmd_test2" "go        2" (Go 1);
    make_test_command "cmd_test3" "     go    7  " 
      (Go 6);
    make_test_command "cmd_test14" "help" Help;
    make_test_command_malformed "cmd_test4" "go -1 ";
    make_test_command_malformed "cmd_test5" "oh";
    make_test_command_malformed "cmd_test6" "";
    make_test_command_malformed "cmd_test7" "       ";
    make_test_command_malformed "cmd_test8" "go   ";
    make_test_command_malformed "cmd_test9" "GO";
    make_test_command_malformed "cmd_test10" "1";
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