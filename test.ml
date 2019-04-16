open Command
open Board
open State
open OUnit2

(** [make_test_board_make_move name board column color expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [make_move board column color] *)
let make_test_board_get_as_list
    (name: string)
    (board: Board.t)
    (column: int)
    (color: Board.color)
    (expected_output: Board.t) : test =
  name >:: (fun _ -> assert_equal 
               expected_output (make_move board column color))



let empty_board = Board.empty
let board1 = Board.make_move empty_board 0 R
let board2 = Board.make_move board1 0 B

let board_tests = [

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
    make_test_command "cmd_test1" "go 0" (Go 0);
    make_test_command "cmd_test2" "go        1" (Go 1);
    make_test_command "cmd_test3" "     go    6  " 
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
    command_tests;
  ]

let _ = run_test_tt_main suite