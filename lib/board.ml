open Ship

let number_array_top size = Array.init (size + 1) (fun num -> num)

let create_board size =
  Array.init size (fun num ->
      Array.init (size + 1) (fun index ->
          if index = 0 then number (num + 1) else empty ()))

let can_place_camel board row col camel_length =
  let rec valid num =
    num >= camel_length
    || (board.(row).(col + num) == empty () && valid (num + 1))
  in
  valid 0

let camel_list = ref []

let rec unique_spot board size camel_length =
  let row = 1 + Random.int (size - 2) in
  let col = Random.int (size - camel_length + 1) + 1 in
  if
    can_place_camel board row col camel_length
    && List.mem (row, col) !camel_list = false
    && List.mem (row, col + 1) !camel_list = false
  then
    let _ = camel_list := (row, col) :: !camel_list in
    (row, col)
  else unique_spot board size camel_length

let place_ship board camel_length =
  let size = Array.length board in
  let row, col = unique_spot board size camel_length in
  for i = 0 to camel_length - 1 do
    board.(row).(col + i) <- empty ()
  done;
  if camel_length = 2 then ((row + 1, col), (row + 1, col + 1))
  else ((row + 1, col), (row + 1, col))

let print_board board =
  Array.iter
    (fun row ->
      Array.iter (fun cell -> print_string (Ship.to_string cell)) row;
      print_newline ())
    board

let update_grid_one board (row, col) (guess_x, guess_y) =
  if (row, col) = (guess_x, guess_y) then board.(row - 1).(col) <- one ()
  else board.(row - 1).(col) <- incorrect ();
  board

let update_grid_two board (row1, col1) (row2, col2) (guess_x, guess_y) =
  if (row1, col1) = (guess_x, guess_y) || (row2, col2) = (guess_x, guess_y) then
    board.(guess_x - 1).(guess_y) <- two ()
  else board.(guess_x - 1).(guess_y) <- incorrect ();
  board

let check_guess (row, col) board_size =
  row >= 1 && col >= 1 && row <= board_size && col <= board_size

type player = {
  board : Ship.t array array;
  mutable two_caravan_guessed : bool;
  mutable one_caravan_guessed : bool;
  mutable guessed_positions : (int * int) list;
}

let create_new_player () =
  {
    board = create_board 8;
    two_caravan_guessed = false;
    one_caravan_guessed = false;
    guessed_positions = [];
  }

let print_numbers_top array_num =
  let () = print_string " " in
  Array.iter (fun x -> print_string (string_of_int x ^ "  ")) array_num;
  print_newline ()

let print_hint_1 (c, d) =
  print_endline
    ("Camel caravan of length 1 placed at position: (" ^ string_of_int c ^ ", "
   ^ string_of_int d ^ ")")

let print_hint_2 (c, d) (x, y) =
  print_endline
    ("Camel caravan of length 1 placed at position: (" ^ string_of_int c ^ ", "
   ^ string_of_int d ^ ")");
  print_endline
    ("First camel of the caravan of length 2 placed at position: ("
   ^ string_of_int x ^ ", " ^ string_of_int y ^ ")")

let print_answer (x, y) (a, b) (c, d) =
  print_endline
    ("Camel caravan of length 1 placed at position: (" ^ string_of_int c ^ ", "
   ^ string_of_int d ^ ")");
  print_endline
    ("First camel of the caravan of length 2 placed at position: ("
   ^ string_of_int x ^ ", " ^ string_of_int y ^ ")");
  print_endline
    ("The second camel of the caravan of length 2 placed at position: ("
   ^ string_of_int a ^ ", " ^ string_of_int b ^ ")")

let proximity_hint (guess_x, guess_y) camels =
  let closest_camel =
    List.fold_left
      (fun (min_dist, hint) (cx, cy) ->
        let dx = abs (guess_x - cx) in
        let dy = abs (guess_y - cy) in
        let dist = dx + dy in
        let new_hint =
          match (dx, dy) with
          | 0, 0 -> "You hit one of the camels exactly!"
          | 0, _ when dy = 1 -> "Hot! Camel is 1 column away."
          | _, 0 when dx = 1 -> "Hot! Camel is 1 row away."
          | _ ->
              Printf.sprintf "Warm. Camel is %d rows and %d columns away." dx dy
        in
        if dist < min_dist then (dist, new_hint) else (min_dist, hint))
      (max_int, "") camels
  in
  snd closest_camel

type guess_type =
  | Coordinates of int * int
  | Hint1
  | Hint2
  | FullAnswer
  | InvalidInput

let read_user_guess ?input () =
  let input =
    match input with
    | Some s -> s
    | None ->
        print_string
          "Enter your guess as 'row,col' or type 'hint1' or 'hint2' or \
           'answer': ";
        read_line ()
  in
  match input with
  | "hint1" -> Some Hint1
  | "hint2" -> Some Hint2
  | "answer" -> Some FullAnswer
  | _ -> (
      match String.split_on_char ',' input with
      | [ row; col ] -> (
          try Some (Coordinates (int_of_string row, int_of_string col))
          with _ -> Some InvalidInput)
      | _ -> Some InvalidInput)

let end_game = ref false

let get_player_name current_player =
  if current_player = 1 then "Player 1" else "Player 2"

let print_turn_info player_name opponent =
  print_endline (player_name ^ "'s Turn:");
  let number_array = number_array_top 8 in
  print_numbers_top number_array;
  print_board opponent.board

let handle_hint hint_type (c, d) (x, y) (a, b) =
  match hint_type with
  | Hint1 -> print_hint_1 (c, d)
  | Hint2 -> print_hint_2 (c, d) (x, y)
  | FullAnswer -> print_answer (x, y) (a, b) (c, d)
  | _ -> ()

let check_winner player =
  player.one_caravan_guessed && player.two_caravan_guessed

let update_player_guess player opponent (row, col) (x, y) (a, b) (c, d) =
  let update_guess_message message =
    print_endline message;
    if not player.one_caravan_guessed then
      print_endline
        "You still need to guess the position of the one camel caravan."
    else if not player.two_caravan_guessed then
      print_endline
        "You still need to guess the second position of the two camel caravan."
  in
  let new_board =
    if (row, col) = (x, y) then (
      update_guess_message
        "You guessed the first position of the two camel caravan.";
      if List.mem (a, b) player.guessed_positions then
        player.two_caravan_guessed <- true;
      player.guessed_positions <- (row, col) :: player.guessed_positions;
      update_grid_two opponent.board (x, y) (a, b) (row, col))
    else if (row, col) = (a, b) then (
      update_guess_message
        "You guessed the second position of the two camel caravan!";
      if List.mem (x, y) player.guessed_positions then
        player.two_caravan_guessed <- true;
      player.guessed_positions <- (row, col) :: player.guessed_positions;
      update_grid_two opponent.board (x, y) (a, b) (row, col))
    else (
      print_endline "You guessed the position of the one camel caravan.";
      player.one_caravan_guessed <- true;
      player.guessed_positions <- (row, col) :: player.guessed_positions;
      update_grid_one opponent.board (c, d) (row, col))
  in
  new_board

let rec handle_guess player opponent current_player (row, col) (x, y) (a, b)
    (c, d) =
  if check_guess (row, col) (Array.length player.board) then (
    let filtered_camels =
      List.filter
        (fun (cx, cy) -> not (List.mem (cx, cy) player.guessed_positions))
        [ (x, y); (a, b); (c, d) ]
    in
    let hint = proximity_hint (row, col) filtered_camels in
    print_endline "";
    print_endline "Proximity Hint: ";
    print_endline hint;
    let new_board =
      if (row, col) = (x, y) || (row, col) = (a, b) || (row, col) = (c, d) then
        update_player_guess player opponent (row, col) (x, y) (a, b) (c, d)
      else (
        print_endline "The camel is not here! Guess again.";
        update_grid_two opponent.board (x, y) (a, b) (row, col))
    in
    print_endline "";
    print_endline ("Feedback for " ^ get_player_name current_player ^ ":");
    let number_array = number_array_top 8 in
    print_numbers_top number_array;
    print_board new_board;
    if check_winner player then (
      end_game := true;
      camel_list := [];
      print_endline
        (get_player_name current_player
        ^ " guessed all of their camels! Congrats!!!"))
    else if not !end_game then
      make_guess opponent player
        (if current_player = 1 then 2 else 1)
        (Array.length player.board)
        (x, y) (a, b) (c, d))
  else (
    print_endline
      "Invalid guess! Make sure your coordinates are within the board range. \
       Guess again.";
    if not !end_game then
      make_guess player opponent current_player
        (Array.length player.board)
        (x, y) (a, b) (c, d))

and make_guess player opponent current_player board_size (x, y) (a, b) (c, d) =
  let player_name = get_player_name current_player in
  if not !end_game then (
    print_turn_info player_name opponent;
    match read_user_guess () with
    | Some Hint1 ->
        handle_hint Hint1 (c, d) (x, y) (a, b);
        if not !end_game then
          make_guess player opponent current_player board_size (x, y) (a, b)
            (c, d)
    | Some Hint2 ->
        handle_hint Hint2 (c, d) (x, y) (a, b);
        if not !end_game then
          make_guess player opponent current_player board_size (x, y) (a, b)
            (c, d)
    | Some FullAnswer ->
        handle_hint FullAnswer (c, d) (x, y) (a, b);
        if not !end_game then
          make_guess player opponent current_player board_size (x, y) (a, b)
            (c, d)
    | Some (Coordinates (row, col)) ->
        handle_guess player opponent current_player (row, col) (x, y) (a, b)
          (c, d);
        if (not !end_game) && not (check_winner player) then
          make_guess opponent player
            (if current_player = 1 then 2 else 1)
            board_size (x, y) (a, b) (c, d)
    | Some InvalidInput | None ->
        print_endline
          "Invalid guess! Make sure your coordinates are within the board \
           range. Guess again.";
        if not !end_game then
          make_guess player opponent current_player board_size (x, y) (a, b)
            (c, d))
