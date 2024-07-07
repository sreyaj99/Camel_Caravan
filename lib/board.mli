val number_array_top : int -> int array
(** [number_array_top size] returns an array of integers from 0 to [size]. 
    This is used to create the top line of the grid that shows the column 
    numbers for the player. *)

val create_board : int -> Ship.t array array
(** [create_board size] returns a 2D array of Ships that creates an empty game 
    board for the player to play on. *)

val can_place_camel : Ship.t array array -> int -> int -> int -> bool
(** [can_place_camel board row col camel_length] returns true if the camel 
    caravan of a specific length can be placed in the grid at the specified 
    row and col. *)

val unique_spot : Ship.t array array -> int -> int -> int * int
(** [unique_spot board size camel_length] returns a tuple representing a valid 
    and unique spot on the board where a camel caravan of the specified length 
    can be placed. *)

val place_ship : Ship.t array array -> int -> (int * int) * (int * int)
(** [place_ship board camel_length] places a camel caravan of the specified 
    length at a random valid location on the board and returns the positions 
    of the camels. *)

val print_board : Ship.t array array -> unit
(** [print_board board] prints out the entire game board cell by cell. *)

val update_grid_one :
  Ship.t array array -> int * int -> int * int -> Ship.t array array
(** [update_grid_one board (row, col) (guess_x, guess_y)] updates the grid 
    with the correct emoji if there is a single caravan of camels at the 
    guessed position. *)

val update_grid_two :
  Ship.t array array ->
  int * int ->
  int * int ->
  int * int ->
  Ship.t array array
(** [update_grid_two board (row1, col1) (row2, col2) (guess_x, guess_y)] 
    updates the grid with the correct emoji if there is a caravan of two 
    camels at the guessed position. *)

val check_guess : int * int -> int -> bool
(** [check_guess (row, col) board_size] checks if the guessed coordinates 
    are within the valid range of the board. *)

type player = {
  board : Ship.t array array;
  mutable two_caravan_guessed : bool;
  mutable one_caravan_guessed : bool;
  mutable guessed_positions : (int * int) list;
}
(** Type representing a player with a game board, flags indicating if caravans 
    have been guessed, and a list of guessed positions. *)

val create_new_player : unit -> player
(** [create_new_player ()] returns a new player with an initialized game board 
    and empty guess flags. *)

val print_numbers_top : int array -> unit
(** [print_numbers_top array_num] prints the top row of numbers for the game 
    board. *)

val print_hint_1 : int * int -> unit
(** [print_hint_1 (c, d)] prints a hint for a camel caravan of length 1 placed 
    at the specified position. *)

val print_hint_2 : int * int -> int * int -> unit
(** [print_hint_2 (c, d) (x, y)] prints hints for both a camel caravan of 
    length 1 and the first camel of a caravan of length 2. *)

val print_answer : int * int -> int * int -> int * int -> unit
(** [print_answer (x, y) (a, b) (c, d)] prints the positions of all camels 
    on the board. *)

val proximity_hint : int * int -> (int * int) list -> string
(** [proximity_hint (guess_x, guess_y) camels] returns a hint indicating the 
    proximity of the closest camel to the guessed coordinates. *)

type guess_type =
  | Coordinates of int * int
  | Hint1
  | Hint2
  | FullAnswer
  | InvalidInput
      (** Type representing the different kinds of guesses a player can make. *)

val read_user_guess : ?input:string -> unit -> guess_type option
(** [read_user_guess ?input ()] reads the user's guess from the input. If no 
    input is provided, it prompts the user to enter a guess. *)

val end_game : bool ref
(** [end_game] is a reference to a boolean indicating if the game has ended. *)

val get_player_name : int -> string
(** [get_player_name current_player] returns the name of the player based on 
    the current player number. *)

val print_turn_info : string -> player -> unit
(** [print_turn_info player_name opponent] prints the turn information for the 
    current player and displays the opponent's board. *)

val handle_hint : guess_type -> int * int -> int * int -> int * int -> unit
(** [handle_hint hint_type (c, d) (x, y) (a, b)] handles printing the 
    appropriate hint based on the guess type. *)

val update_player_guess :
  player ->
  player ->
  int * int ->
  int * int ->
  int * int ->
  int * int ->
  Ship.t array array
(** [update_player_guess player opponent (row, col) (x, y) (a, b) (c, d)] 
    updates the player's guess and the opponent's board based on the guessed 
    coordinates. *)

val handle_guess :
  player ->
  player ->
  int ->
  int * int ->
  int * int ->
  int * int ->
  int * int ->
  unit
(** [handle_guess player opponent current_player (row, col) (x, y) (a, b) (c, d)] 
    handles the logic when a valid guess is made. *)

val make_guess :
  player -> player -> int -> int -> int * int -> int * int -> int * int -> unit
(** [make_guess player opponent current_player board_size (x, y) (a, b) (c, d)] 
    allows the current player to make a guess and updates the game state 
    accordingly. *)

val check_winner : player -> bool
(** [check_winner player] returns true if the player has guessed all their 
    camels correctly. *)
