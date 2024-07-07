open OUnit2
open QCheck
open Battleship.Board
open Battleship.Ship

(* Utility function to capture printed output *)

let capture_output f =
  let old_stdout = Unix.dup Unix.stdout in

  let temp_file = Filename.temp_file "stdout_capture" ".txt" in

  let temp_fd = Unix.openfile temp_file [ Unix.O_RDWR ] 0o640 in

  Unix.dup2 temp_fd Unix.stdout;

  f ();

  Unix.dup2 old_stdout Unix.stdout;

  Unix.close temp_fd;

  let ic = open_in temp_file in

  let output = really_input_string ic (in_channel_length ic) in

  close_in ic;

  Sys.remove temp_file;

  output

(* Function to check if a string contains a substring using Str module *)

let contains_substring s1 s2 =
  try
    ignore (Str.search_forward (Str.regexp_string s2) s1 0);

    true
  with Not_found -> false

(* Ship to string tests *)

let test_empty_to_string _ = assert_equal " 🌵" (to_string (empty ()))
let test_number_to_string_1 _ = assert_equal " 1" (to_string (number 1))
let test_number_to_string_5 _ = assert_equal " 5" (to_string (number 5))
let test_one_to_string _ = assert_equal " 🐫" (to_string (one ()))
let test_incorrect_to_string _ = assert_equal " ❌" (to_string (incorrect ()))
let test_two_to_string _ = assert_equal " 🐫" (to_string (two ()))

(* Create board tests *)

let test_create_board_size_8 _ =
  let board = create_board 8 in

  assert_equal (Array.length board) 8

let test_create_board_size_10 _ =
  let board = create_board 10 in

  assert_equal (Array.length board) 10

let test_create_board_row_size_8 _ =
  let board = create_board 8 in

  Array.iter (fun row -> assert_equal (Array.length row) 9) board

let test_create_board_row_size_10 _ =
  let board = create_board 10 in

  Array.iter (fun row -> assert_equal (Array.length row) 11) board

(* Check guess tests *)

let test_valid_guess_3_5 _ = assert_bool "Valid guess" (check_guess (3, 5) 8)
let test_valid_guess_1_1 _ = assert_bool "Valid guess" (check_guess (1, 1) 8)

let test_invalid_guess_row_out_of_bounds_9_5 _ =
  assert_bool "Invalid guess (row out of bounds)" (not (check_guess (9, 5) 8))

let test_invalid_guess_row_out_of_bounds_0_5 _ =
  assert_bool "Invalid guess (row out of bounds)" (not (check_guess (0, 5) 8))

let test_invalid_guess_col_out_of_bounds_3_10 _ =
  assert_bool "Invalid guess (column out of bounds)"
    (not (check_guess (3, 10) 8))

let test_invalid_guess_col_out_of_bounds_3_0 _ =
  assert_bool "Invalid guess (column out of bounds)"
    (not (check_guess (3, 0) 8))

let test_invalid_guess_negative_row _ =
  assert_bool "Invalid guess (negative row)" (not (check_guess (-1, 5) 8))

let test_invalid_guess_negative_col _ =
  assert_bool "Invalid guess (negative column)" (not (check_guess (3, -1) 8))

(* Update grid tests *)

let test_update_grid_one_correct _ =
  let board = create_board 8 in

  let new_board = update_grid_one board (2, 3) (2, 3) in

  assert_equal new_board.(1).(3) (one ())

let test_update_grid_one_incorrect _ =
  let board = create_board 8 in

  let new_board = update_grid_one board (2, 3) (4, 5) in

  assert_equal new_board.(1).(3) (incorrect ())

let test_update_grid_two_first_pos _ =
  let board = create_board 8 in

  let new_board = update_grid_two board (1, 2) (1, 3) (1, 2) in

  assert_equal new_board.(0).(2) (two ())

let test_update_grid_two_second_pos _ =
  let board = create_board 8 in

  let new_board = update_grid_two board (1, 2) (1, 3) (1, 3) in

  assert_equal new_board.(0).(3) (two ())

let test_update_grid_two_incorrect_pos _ =
  let board = create_board 8 in

  let new_board = update_grid_two board (1, 2) (1, 3) (1, 4) in

  assert_equal new_board.(0).(4) (incorrect ())

(* Place ship tests *)

let test_place_ship_two_camels _ =
  let board = create_board 8 in

  let pos1, pos2 = place_ship board 2 in

  let row1, col1 = pos1 in

  let row2, col2 = pos2 in

  assert_equal board.(row1 - 1).(col1) (empty ());

  assert_equal board.(row2 - 1).(col2) (empty ())

let test_place_ship_two_camels_overlap _ =
  let board = create_board 8 in

  let pos1, pos2 = place_ship board 2 in

  let row1, col1 = pos1 in

  let row2, col2 = pos2 in

  let pos3, pos4 = place_ship board 2 in

  let row3, col3 = pos3 in

  let row4, col4 = pos4 in

  assert_bool "Ships do not overlap"
    ((row1, col1) <> (row3, col3) && (row2, col2) <> (row4, col4))

let test_place_ship_one_camel _ =
  let board = create_board 8 in

  let pos3 = fst (place_ship board 1) in

  let row3, col3 = pos3 in

  assert_equal board.(row3 - 1).(col3) (empty ())

(* Can place camel tests *)

let test_can_place_camel_valid _ =
  let board = create_board 8 in

  assert_bool "Can place camel" (can_place_camel board 1 1 2)

let test_can_place_camel_invalid _ =
  let board = create_board 8 in

  board.(1).(1) <- one ();

  assert_bool "Cannot place camel" (not (can_place_camel board 1 1 2))

let test_can_place_camel_edge _ =
  let board = create_board 8 in

  assert_bool "Can place camel at edge" (can_place_camel board 7 7 1)

(* Unique spot tests *)

let test_unique_spot_row _ =
  let board = create_board 8 in

  let row, _ = unique_spot board 8 2 in

  assert_bool "Unique spot row" (row >= 1 && row <= 7)

let test_unique_spot_col _ =
  let board = create_board 8 in

  let _, col = unique_spot board 8 2 in

  assert_bool "Unique spot col" (col >= 1 && col <= 7)

(* Number array top tests *)

let test_number_array_top_5 _ =
  let arr = number_array_top 5 in

  assert_equal [| 0; 1; 2; 3; 4; 5 |] arr

let test_number_array_top_10 _ =
  let arr = number_array_top 10 in

  assert_equal [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |] arr

(* Create new player tests *)

let test_create_new_player_board_size_8 _ =
  let player = create_new_player () in

  assert_equal 8 (Array.length player.board)

let test_create_new_player_board_size_10 _ =
  let player = create_new_player () in

  assert_equal 8 (Array.length player.board)

(* Change to match the create_board size which is always 8 *)

let test_create_new_player_board_row_size _ =
  let player = create_new_player () in

  Array.iter (fun row -> assert_equal 9 (Array.length row)) player.board

let test_create_new_player_initial_two_caravan_guessed _ =
  let player = create_new_player () in

  assert_bool "Initial two_caravan_guessed" (not player.two_caravan_guessed)

let test_create_new_player_initial_one_caravan_guess _ =
  let player = create_new_player () in

  assert_bool "Initial one_caravan_guess" (not player.one_caravan_guessed)

(* Proximity hint tests *)

let test_proximity_hint_hot_row _ =
  let hint = proximity_hint (3, 3) [ (4, 3); (6, 5); (6, 6) ] in

  assert_equal "Hot! Camel is 1 row away." hint

let test_proximity_hint_warm _ =
  let hint = proximity_hint (3, 3) [ (1, 1); (2, 2); (5, 5) ] in

  assert_equal "Warm. Camel is 1 rows and 1 columns away." hint

let test_proximity_hint_exact _ =
  let hint = proximity_hint (3, 3) [ (3, 3); (4, 5); (4, 6) ] in

  assert_equal "You hit one of the camels exactly!" hint

let test_proximity_hint_hot_col _ =
  let hint = proximity_hint (3, 3) [ (3, 4); (4, 5); (4, 6) ] in

  assert_equal "Hot! Camel is 1 column away." hint

(* Read user guess tests *)

let test_read_user_guess_coords _ =
  assert_equal (Some (Coordinates (1, 2))) (read_user_guess ~input:"1,2" ())

let test_read_user_guess_hint1 _ =
  assert_equal (Some Hint1) (read_user_guess ~input:"hint1" ())

let test_read_user_guess_hint2 _ =
  assert_equal (Some Hint2) (read_user_guess ~input:"hint2" ())

let test_read_user_guess_answer _ =
  assert_equal (Some FullAnswer) (read_user_guess ~input:"answer" ())

let test_read_user_guess_invalid _ =
  assert_equal (Some InvalidInput) (read_user_guess ~input:"invalid" ())

(* More edge cases for read user guess *)

let test_read_user_guess_invalid_format _ =
  assert_equal (Some InvalidInput) (read_user_guess ~input:"1-2" ())

let test_read_user_guess_empty _ =
  assert_equal (Some InvalidInput) (read_user_guess ~input:"" ())

let test_read_user_guess_no_comma _ =
  assert_equal (Some InvalidInput) (read_user_guess ~input:"12" ())

let test_read_user_guess_extra_comma _ =
  assert_equal (Some InvalidInput) (read_user_guess ~input:"1,2,3" ())

(* get_player_name tests *)

let test_get_player_name_1 _ = assert_equal "Player 1" (get_player_name 1)
let test_get_player_name_2 _ = assert_equal "Player 2" (get_player_name 2)

(* print_turn_info tests *)

let test_print_turn_info _ =
  let player = create_new_player () in

  let output = capture_output (fun () -> print_turn_info "Player 1" player) in

  assert (contains_substring output "Player 1's Turn:")

(* handle_hint tests *)

let test_handle_hint_1 _ =
  let output =
    capture_output (fun () -> handle_hint Hint1 (1, 1) (2, 2) (3, 3))
  in

  assert (
    contains_substring output "Camel caravan of length 1 placed at position")

let test_handle_hint_2 _ =
  let output =
    capture_output (fun () -> handle_hint Hint2 (1, 1) (2, 2) (3, 3))
  in

  assert (
    contains_substring output
      "First camel of the caravan of length 2 placed at position")

let test_handle_hint_full_answer _ =
  let output =
    capture_output (fun () -> handle_hint FullAnswer (1, 1) (2, 2) (3, 3))
  in

  assert (
    contains_substring output "Camel caravan of length 1 placed at position")

let test_update_player_guess_correct _ =
  let player = create_new_player () in

  let opponent = create_new_player () in

  let row, col = (2, 3) in

  let x, y = (1, 2) in

  let a, b = (3, 4) in

  let c, d = (5, 6) in

  let _ = update_player_guess player opponent (row, col) (x, y) (a, b) (c, d) in

  assert (List.mem (row, col) player.guessed_positions);

  if (row, col) = (x, y) then
    assert (
      opponent.board.(row).(col) = one () || opponent.board.(row).(col) = two ())
  else if (row, col) = (a, b) then
    assert (
      opponent.board.(a).(b) = one () || opponent.board.(row).(col) = two ())
  else if (row, col) = (c, d) then
    assert (
      opponent.board.(c).(d) = one () || opponent.board.(row).(col) = two ())

let test_update_player_guess_incorrect _ =
  let player = create_new_player () in

  let opponent = create_new_player () in

  let row, col = (2, 3) in

  let x, y = (1, 2) in

  let a, b = (3, 4) in

  let c, d = (5, 6) in

  let _ = update_player_guess player opponent (row, col) (x, y) (a, b) (c, d) in

  if (row, col) = (x, y) then
    assert (
      opponent.board.(row).(col) <> incorrect ()
      || opponent.board.(row).(col) <> incorrect ())
  else if (row, col) = (a, b) then
    assert (
      opponent.board.(a).(b) <> incorrect ()
      || opponent.board.(row).(col) <> incorrect ())
  else if (row, col) = (c, d) then
    assert (
      opponent.board.(c).(d) <> incorrect ()
      || opponent.board.(row).(col) <> incorrect ())

let test_update_player_guess_correct_one_camel _ =
  let player = create_new_player () in

  let opponent = create_new_player () in

  let x, y = (2, 3) in

  let a, b = (3, 4) in

  let c, d = (5, 6) in

  let row, col = (5, 6) in

  let new_board =
    update_player_guess player opponent (row, col) (x, y) (a, b) (c, d)
  in

  assert (List.mem (row, col) player.guessed_positions);

  assert_equal new_board.(row - 1).(col) (one ());

  assert_bool "One camel guessed" player.one_caravan_guessed

let test_update_player_guess_correct_two_camel_first _ =
  let player = create_new_player () in

  let opponent = create_new_player () in

  let x, y = (2, 3) in

  let a, b = (3, 4) in

  let c, d = (5, 6) in

  let row, col = (2, 3) in

  let new_board =
    update_player_guess player opponent (row, col) (x, y) (a, b) (c, d)
  in

  assert (List.mem (row, col) player.guessed_positions);

  assert_equal new_board.(row - 1).(col) (two ());

  assert_bool "First camel of two guessed" (not player.two_caravan_guessed)

let test_update_player_guess_correct_two_camel_second _ =
  let player = create_new_player () in

  let opponent = create_new_player () in

  let x, y = (2, 3) in

  let a, b = (3, 4) in

  let c, d = (5, 6) in

  player.guessed_positions <- [ (2, 3) ];

  let row, col = (3, 4) in

  let new_board =
    update_player_guess player opponent (row, col) (x, y) (a, b) (c, d)
  in

  assert (List.mem (row, col) player.guessed_positions);

  assert_equal new_board.(row - 1).(col) (two ());

  assert_bool "Second camel of two guessed" player.two_caravan_guessed

let test_can_place_camel_boundary_top_left _ =
  let board = create_board 8 in

  assert_bool "Can place camel at the top left corner"
    (can_place_camel board 1 1 1)

let test_can_place_camel_boundary_top_left_size_2 _ =
  let board = create_board 8 in

  assert_bool "Can place camel of size 2 at the top left corner"
    (can_place_camel board 1 1 2)

let test_can_place_camel_boundary_bottom_right_size_2 _ =
  let board = create_board 8 in

  assert_bool "Can place camel of size 2 at the bottom right corner"
    (can_place_camel board 7 7 2)

let test_update_player_guess_one_caravan_correct _ =
  let player = create_new_player () in
  player.one_caravan_guessed <- false;
  let opponent = create_new_player () in
  let row, col = (2, 3) in
  let x, y = (1, 3) in
  let a, b = (3, 4) in
  let c, d = (2, 3) in
  let _ = update_player_guess player opponent (row, col) (x, y) (a, b) (c, d) in
  assert (List.mem (row, col) player.guessed_positions);
  assert (player.one_caravan_guessed = true)

let test_update_player_guess_two_caravan_first_position_correct _ =
  let player = create_new_player () in
  player.two_caravan_guessed <- false;
  let opponent = create_new_player () in
  let row, col = (2, 3) in
  let x, y = (2, 3) in
  let a, b = (3, 4) in
  let c, d = (1, 3) in
  let _ = update_player_guess player opponent (row, col) (x, y) (a, b) (c, d) in
  assert (List.mem (row, col) player.guessed_positions);
  assert (player.two_caravan_guessed = false)

let test_update_player_guess_two_caravan_second_position_correct _ =
  let player = create_new_player () in
  player.two_caravan_guessed <- false;
  let opponent = create_new_player () in
  let row, col = (2, 3) in
  let x, y = (2, 3) in
  let a, b = (4, 3) in
  let c, d = (1, 3) in
  let _ = update_player_guess player opponent (row, col) (x, y) (a, b) (c, d) in
  assert (List.mem (row, col) player.guessed_positions);
  let row, col = (4, 3) in
  let x, y = (2, 3) in
  let a, b = (4, 3) in
  let c, d = (1, 3) in
  let _ = update_player_guess player opponent (row, col) (x, y) (a, b) (c, d) in
  assert (List.mem (row, col) player.guessed_positions);
  assert (player.two_caravan_guessed = true)

let test_update_player_guess_empty _ =
  let player = create_new_player () in
  let opponent = create_new_player () in
  let row, col = (7, 3) in
  let x, y = (7, 3) in
  let a, b = (4, 3) in
  let c, d = (1, 3) in
  let _ = update_player_guess player opponent (row, col) (x, y) (a, b) (c, d) in
  assert (List.mem (row, col) player.guessed_positions);
  assert (opponent.board.(x).(y) = empty ())

let test_place_ship _ =
  let board = create_board 8 in
  let (row1, col1), (row2, col2) = place_ship board 2 in
  assert (row1 = row2);
  assert_equal 2 (col2 - col1 + 1);
  assert (board.(row1 - 1).(col1) = empty ());
  assert (board.(row1 - 1).(col2) = empty ())

let test_check_guess_nonvalid_int_greater _ =
  assert (not (check_guess (9, 9) 8));
  assert (not (check_guess (9, 0) 8));
  assert (not (check_guess (10, 1) 8))

let test_check_guess_nonvalid_nonint_less _ =
  assert (not (check_guess (0, 1) 8));
  assert (not (check_guess (1, 0) 8));
  assert (not (check_guess (0, 0) 8));
  assert (not (check_guess (-1, -2) 8))

let test_check_guess_valid_ _ =
  assert (check_guess (1, 1) 8);
  assert (check_guess (1, 8) 8)

let test_update_grid_one _ =
  let board = create_board 8 in
  let updated_board = update_grid_one board (1, 1) (1, 1) in
  assert (updated_board.(0).(1) = one ());
  let updated_board = update_grid_one board (1, 1) (2, 2) in
  assert (updated_board.(0).(1) = incorrect ())

let test_update_grid_two _ =
  let board = create_board 8 in
  let updated_board = update_grid_two board (1, 1) (1, 2) (1, 1) in
  assert (updated_board.(0).(1) = two ());
  let updated_board = update_grid_two board (1, 1) (1, 2) (2, 2) in
  assert (updated_board.(1).(2) = incorrect ())

let camel_is_placed_correctly_test (grid, camel_length) =
  let board = create_board grid in
  let row = 1 + Random.int (grid - 2) in
  let col = Random.int (grid - camel_length + 1) + 1 in
  let can_place = can_place_camel board row col camel_length in
  if can_place then
    let rec check_placement_validity num =
      num >= camel_length
      || board.(row).(col + num) = empty ()
         && check_placement_validity (num + 1)
    in
    check_placement_validity 0
  else true

(* Adapted from Chatgpt, accesssed 5/16/24*)
let camel_generator = Gen.pair (Gen.int_range 3 10) (Gen.int_range 1 3)

let test_can_place_camel =
  Test.make ~count:1000 ~name:"camel placement test"
    (QCheck.make camel_generator)
    camel_is_placed_correctly_test

let ounit_test_can_place_camel =
  QCheck_runner.to_ounit2_test test_can_place_camel

let everything_test_1 _ =
  let player1 = create_new_player () in
  let player2 = create_new_player () in
  let camel1_length2, camel2_length2 = place_ship player2.board 2 in
  let x, y = (camel1_length2, camel2_length2) in
  let camel1 = fst (place_ship player2.board 1) in
  let camel2_pos = (fst camel1, snd camel1) in
  assert_equal false player1.two_caravan_guessed;
  assert_equal false player1.one_caravan_guessed;
  assert_equal [] player1.guessed_positions;
  let row, col = x in
  let _ = update_player_guess player1 player2 (row, col) x y camel2_pos in
  assert_equal true (List.mem (row, col) player1.guessed_positions);
  assert_equal false player1.two_caravan_guessed;
  assert_equal false player1.one_caravan_guessed;
  let row2, col2 = y in
  let _ = update_player_guess player1 player2 (row2, col2) x y camel2_pos in
  assert_equal true (List.mem (row2, col2) player1.guessed_positions);
  assert_equal true player1.two_caravan_guessed;
  assert_equal false player1.one_caravan_guessed;
  let row3, col3 = camel2_pos in
  let _ = update_player_guess player1 player2 (row3, col3) x y camel2_pos in
  assert_equal true (List.mem (row3, col3) player1.guessed_positions);
  assert_equal true player1.two_caravan_guessed;
  assert_equal true player1.one_caravan_guessed;
  assert_equal true (check_winner player1)

let everything_test_2 _ =
  let player1 = create_new_player () in
  let player2 = create_new_player () in
  let camel1_length2, camel2_length2 = place_ship player1.board 2 in
  let x, y = (camel1_length2, camel2_length2) in
  let camel1 = fst (place_ship player1.board 1) in
  let camel2_pos = (fst camel1, snd camel1) in
  assert_equal false player2.two_caravan_guessed;
  assert_equal false player2.one_caravan_guessed;
  assert_equal [] player2.guessed_positions;
  let row3, col3 = camel2_pos in
  let _ = update_player_guess player2 player1 (row3, col3) x y camel2_pos in
  assert_equal true (List.mem (row3, col3) player2.guessed_positions);
  assert_equal false player2.two_caravan_guessed;
  assert_equal true player2.one_caravan_guessed;
  let row, col = x in
  let _ = update_player_guess player2 player1 (row, col) x y camel2_pos in
  assert_equal true (List.mem (row, col) player2.guessed_positions);
  assert_equal false player2.two_caravan_guessed;
  assert_equal true player2.one_caravan_guessed;
  let row2, col2 = y in
  let _ = update_player_guess player2 player1 (row2, col2) x y camel2_pos in
  assert_equal true (List.mem (row2, col2) player2.guessed_positions);
  assert_equal true player2.two_caravan_guessed;
  assert_equal true player2.one_caravan_guessed;
  assert_equal true (check_winner player2);
  assert_equal false player1.two_caravan_guessed;
  assert_equal false player1.one_caravan_guessed;
  assert_equal [] player1.guessed_positions

let test_place_ship_overlap _ =
  let board = create_board 8 in
  let _ = place_ship board 2 in
  let pos1, pos2 = place_ship board 2 in
  let row1, col1 = pos1 in
  let row2, col2 = pos2 in
  assert (board.(row1 - 1).(col1) = empty ());
  assert (board.(row2 - 1).(col2) = empty ())

let test_place_single_camel_no_overlap _ =
  let board = create_board 8 in

  let pos1 = fst (place_ship board 1) in
  let row1, col1 = pos1 in
  board.(row1 - 1).(col1) <- one ();

  let pos2 = fst (place_ship board 1) in
  let row2, col2 = pos2 in
  assert_bool "Second camel does not overlap with the first camel"
    (board.(row2 - 1).(col2) = empty ());
  board.(row2 - 1).(col2) <- one ()

let suite =
  "Battleship Board Tests"
  >::: [
         (* Ship to string tests *)
         "test_empty_to_string" >:: test_empty_to_string;
         "test_number_to_string_1" >:: test_number_to_string_1;
         "test_number_to_string_5" >:: test_number_to_string_5;
         "test_one_to_string" >:: test_one_to_string;
         "test_incorrect_to_string" >:: test_incorrect_to_string;
         "test_two_to_string" >:: test_two_to_string;
         (* Create board tests *)
         "test_create_board_size_8" >:: test_create_board_size_8;
         "test_create_board_size_10" >:: test_create_board_size_10;
         "test_create_board_row_size_8" >:: test_create_board_row_size_8;
         "test_create_board_row_size_10" >:: test_create_board_row_size_10;
         (* Check guess tests *)
         "test_valid_guess_3_5" >:: test_valid_guess_3_5;
         "test_valid_guess_1_1" >:: test_valid_guess_1_1;
         "test_invalid_guess_row_out_of_bounds_9_5"
         >:: test_invalid_guess_row_out_of_bounds_9_5;
         "test_invalid_guess_row_out_of_bounds_0_5"
         >:: test_invalid_guess_row_out_of_bounds_0_5;
         "test_invalid_guess_col_out_of_bounds_3_10"
         >:: test_invalid_guess_col_out_of_bounds_3_10;
         "test_invalid_guess_col_out_of_bounds_3_0"
         >:: test_invalid_guess_col_out_of_bounds_3_0;
         "test_invalid_guess_negative_row" >:: test_invalid_guess_negative_row;
         "test_invalid_guess_negative_col" >:: test_invalid_guess_negative_col;
         (* Update grid tests *)
         "test_update_grid_one_correct" >:: test_update_grid_one_correct;
         "test_update_grid_one_incorrect" >:: test_update_grid_one_incorrect;
         "test_update_grid_two_first_pos" >:: test_update_grid_two_first_pos;
         "test_update_grid_two_second_pos" >:: test_update_grid_two_second_pos;
         "test_update_grid_two_incorrect_pos"
         >:: test_update_grid_two_incorrect_pos;
         (* Place ship tests *)
         "test_place_ship_two_camels" >:: test_place_ship_two_camels;
         "test_place_ship_two_camels_overlap"
         >:: test_place_ship_two_camels_overlap;
         "test_place_ship_one_camel" >:: test_place_ship_one_camel;
         (* Can place camel tests *)
         "test_can_place_camel_valid" >:: test_can_place_camel_valid;
         "test_can_place_camel_invalid" >:: test_can_place_camel_invalid;
         "test_can_place_camel_edge" >:: test_can_place_camel_edge;
         (* Unique spot tests *)
         "test_unique_spot_row" >:: test_unique_spot_row;
         "test_unique_spot_col" >:: test_unique_spot_col;
         (* Number array top tests *)
         "test_number_array_top_5" >:: test_number_array_top_5;
         "test_number_array_top_10" >:: test_number_array_top_10;
         (* Create new player tests *)
         "test_create_new_player_board_size_8"
         >:: test_create_new_player_board_size_8;
         "test_create_new_player_board_size_10"
         >:: test_create_new_player_board_size_10;
         "test_create_new_player_board_row_size"
         >:: test_create_new_player_board_row_size;
         "test_create_new_player_initial_two_caravan_guessed"
         >:: test_create_new_player_initial_two_caravan_guessed;
         "test_create_new_player_initial_one_caravan_guess"
         >:: test_create_new_player_initial_one_caravan_guess;
         (* Proximity hint tests *)
         "test_proximity_hint_hot_row" >:: test_proximity_hint_hot_row;
         "test_proximity_hint_warm" >:: test_proximity_hint_warm;
         "test_proximity_hint_exact" >:: test_proximity_hint_exact;
         "test_proximity_hint_hot_col" >:: test_proximity_hint_hot_col;
         (* Read user guess tests *)
         "test_read_user_guess_coords" >:: test_read_user_guess_coords;
         "test_read_user_guess_hint1" >:: test_read_user_guess_hint1;
         "test_read_user_guess_hint2" >:: test_read_user_guess_hint2;
         "test_read_user_guess_answer" >:: test_read_user_guess_answer;
         "test_read_user_guess_invalid" >:: test_read_user_guess_invalid;
         "test_read_user_guess_invalid_format"
         >:: test_read_user_guess_invalid_format;
         "test_read_user_guess_empty" >:: test_read_user_guess_empty;
         "test_read_user_guess_no_comma" >:: test_read_user_guess_no_comma;
         "test_read_user_guess_extra_comma" >:: test_read_user_guess_extra_comma;
         (* get_player_name tests *)
         "test_get_player_name_1" >:: test_get_player_name_1;
         "test_get_player_name_2" >:: test_get_player_name_2;
         (* print_turn_info tests *)
         "test_print_turn_info" >:: test_print_turn_info;
         (* handle_hint tests *)
         "test_handle_hint_1" >:: test_handle_hint_1;
         "test_handle_hint_2" >:: test_handle_hint_2;
         "test_handle_hint_full_answer" >:: test_handle_hint_full_answer;
         "test_update_player_guess_correct" >:: test_update_player_guess_correct;
         "test_update_player_guess_incorrect"
         >:: test_update_player_guess_incorrect;
         "test_update_player_guess_correct_one_camel"
         >:: test_update_player_guess_correct_one_camel;
         "test_update_player_guess_correct_two_camel_first"
         >:: test_update_player_guess_correct_two_camel_first;
         "test_update_player_guess_correct_two_camel_second"
         >:: test_update_player_guess_correct_two_camel_second;
         "test_can_place_camel_boundary_top_left"
         >:: test_can_place_camel_boundary_top_left;
         "test_can_place_camel_boundary_top_left_size_2"
         >:: test_can_place_camel_boundary_top_left_size_2;
         "test_can_place_camel_boundary_bottom_right_size_2"
         >:: test_can_place_camel_boundary_bottom_right_size_2;
         "test_update_player_guess_one_caravan_correct"
         >:: test_update_player_guess_one_caravan_correct;
         "test_update_player_guess_two_caravan_first_position_correct"
         >:: test_update_player_guess_two_caravan_first_position_correct;
         "test_update_player_guess_two_caravan_second_position_correct"
         >:: test_update_player_guess_two_caravan_second_position_correct;
         "test_update_player_guess_empty" >:: test_update_player_guess_empty;
         "test_place_ship" >:: test_place_ship;
         "test_check_guess_nonvalid_int_greater"
         >:: test_check_guess_nonvalid_int_greater;
         "test_check_guess_valid_" >:: test_check_guess_valid_;
         "test_check_guess_valid_nonint_less"
         >:: test_check_guess_nonvalid_nonint_less;
         "test_update_grid_one" >:: test_update_grid_one;
         "test_update_grid_two" >:: test_update_grid_two;
         "everything_test_1" >:: everything_test_1;
         "everything_test_2" >:: everything_test_2;
         ounit_test_can_place_camel;
         "test_place_ship_overlap" >:: test_place_ship_overlap;
         "test_place_single_camel_no_overlap"
         >:: test_place_single_camel_no_overlap;
       ]

let () = run_test_tt_main suite
