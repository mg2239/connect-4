open OUnit2
open Board
open State
open Command

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
      assert_equal [[R]; [B]; []; []; []; []; []] 
        (Board.get_as_list (State.board st3)));
  "current test 1" >:: (fun _ ->
      assert_equal R (State.current_player init_st));
  "current test 2" >:: (fun _ ->
      assert_equal B (State.current_player st2));
  "next test 1" >:: (fun _ ->
      assert_equal R (State.next_player st2));
  "next test 2" >:: (fun _ ->
      assert_equal B (State.next_player init_st));
]

let suite =
  "test suite for A6"  >::: List.flatten [
    state_tests;
  ]

let _ = run_test_tt_main suite