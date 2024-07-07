(* @author Sanjum Sahni (ss3873), Sreya Jonnalagadda (sj652) and
   Srinithi Krishnamoorthy (sk2693) *)

open Battleship.Board

let game_log = Buffer.create 1000

let log_message msg =
  Buffer.add_string game_log (msg ^ "\n");
  print_endline msg

let print_help () =
  log_message "🐪 Welcome to the Camel Caravan Help Manual! 🐪 \n ";
  log_message
    "Welcome to Camel Caravan, an exciting twist on the classic game of \n\
     Battleship. Instead of ships, your mission is to locate lost camels in \n\
     the desert! This guide will provide you with the game rules and \n\
     instructions on how to play. Ready your guesses and find those camels \n\
     before your guesses run out!";

  log_message
    "\n\n\
     🐪 Game Setup 🐪\n\n\
     Camel Caravan is played on a grid-based board using emojis to \n\
     represent different elements of the game. Each player (multi-player \
     version) will have their own board but the same camel placement. Camels \
     are randomly placed on this board. The goal is to guess the exact \
     locations of the camels.\n\n\
    \ ";

  log_message
    "🐪 Objective 🐪\n\n\n\
     The objective of the game is to find all the camels hidden on the board \
     before the other player. You will have a timer to keep track of the \
     duration and this can be reflected in the leaderboard. \n\n\
    \      \n\n\
     🐪 Rules and Gameplay 🐪\n\n\
    \      \n\n\
    \      1. Starting the Game: To start the game the player will type the \
     following into one terminal to begin the gamelog:dune exec bin/main.exe \
     log 127.0.0.1 5000 and the player can then type the following in another \
     new terminal to begin the game on their side: dune exec bin/main.exe \
     users [id] [port] [username1] [username2]. When the game starts, the \
     camels are randomly placed on different locations on your grid. The \
     positions are hidden from each player.  \n\n\
    \      \n\n\
    \      2. Playing Your Turn: On your turn, you will guess a location on the \n\
    \     board where you think a camel is placed. You must specify the row \
     and the column you want to target.\n\n\
    \      \n\n\
    \      3. Making a Guess: Enter the coordinates for your guess in the \
     format `(row, column)`. For example, entering `(3, 5)` will target the \
     cell at row 3, column 5. HINTS ALSO EXIST! You can also enter 'hint1', \
     'hint2' or 'answer' to get the answers to where the camels are. 'hint1' \
     will provide one hint about the first camels location, 'hint2' will \
     provide two hints for two camels, and the 'answer' will give all the \
     coordinates needed to win. \n\
    \    \n\n\
    \      4. Hit or Miss:\n\n\
    \          - If a camel is located at your guessed location, it is a hit \
     The \n\
    \     game will update the board to show an emoji indicating a hit at that \n\
    \     location.\n\n\
    \          - If there is no camel, it is a miss The game will mark this \
     with a \n\
    \     different emoji.\n\n\
    \      \n\n\
    \      5. Game Feedback: After each guess, the game will provide feedback:\n\n\
    \          - Hit: You will see a hit emoji (🐪) at the guessed location.\n\n\
    \          - Miss: A miss emoji (❌) will appear.\n\n\
    \      \n\n\
    \      6. Winning the Game: The game ends when:\n\n\
    \          - Win: You find all three camels before the other player.\n\n\
    \          - Lose: Anotherplayer finishes before you.\n\n\
    \      \n\n\
    \      7. Game Over: At the end of the game, the player can choose to \
     receive a game log and leaderboard log which will be the in the form of a \
     txt file which is saved to the Battleship folder root. The game log will \
     return the time it took to win the game for the current playing pair and \
     the leaderboard log will  keep a log of all the players connected to the \
     main server and their timings as well .\n\n\
    \      \n\n\
     🐪 Tips for Playing 🐪\n\n\
    \      - Pay attention to the feedback from each guess to adjust your \
     strategy.\n\n\
    \      - Try to spread out your guesses to maximize the area you cover.\n\n\
    \      \n\n\
     🐪 Conclusion 🐪\n\n\
    \      \n\n\
    \      Thank you for playing Camel Carvan. We hope this guide helps you \n\
    \     enjoy your gaming experience. Good luck, and happy camel hunting!\n\n\
    \      "

let print_numbers_top array_num =
  let () = print_string " " in
  Array.iter (fun x -> print_string (string_of_int x ^ "  ")) array_num;
  print_newline ()

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

let connected_clients = ref []

let remove_client client_out =
  connected_clients :=
    List.filter (fun (_, out) -> out != client_out) !connected_clients;

  Lwt_io.close client_out

let client_handler client_socket_address (client_in, client_out) =
  connected_clients := (client_socket_address, client_out) :: !connected_clients;

  let addr_str = string_of_sockaddr client_socket_address in

  let%lwt initial_message = Lwt_io.read_line client_in in

  let username1, username2 =
    Scanf.sscanf initial_message "[%s@] and [%s@] have entered the chat."
      (fun u1 u2 -> (u1, u2))
  in

  let%lwt () =
    Lwt_io.printlf "New clients connected: %s and %s (%s)" username1 username2
      addr_str
  in

  let%lwt () =
    Lwt_io.printlf
      "[Game Log] %s and %s have begun playing a game of Caravan of Camels."
      username1 username2
  in
  let t = Unix.gettimeofday () in
  let rec handle_messages () =
    let%lwt message = Lwt_io.read_line client_in in

    let formatted_message = Printf.sprintf "[%s]: %s" username1 message in

    let%lwt () = Lwt_io.printlf "%s" formatted_message in

    handle_messages ()
  in

  Lwt.catch handle_messages (fun e ->
      let%lwt () =
        Lwt_io.printlf
          "[Game Log]: %s and %s have finished playing a game of Caravan of \
           Camels."
          username1 username2
      in

      let%lwt () =
        Lwt_io.printlf "[Game Log]: %s and %s have disconnected (%s): %s"
          username1 username2 addr_str (Printexc.to_string e)
      in

      let execution_time = Unix.gettimeofday () -. t in
      let%lwt () =
        Lwt_io.printlf
          "%s and %s were in Caravan of Camels for this long: %.1fs\n" username1
          username2 execution_time
      in
      remove_client client_out)

let run_server ip port =
  let server_socket_address =
    Unix.ADDR_INET (Unix.inet_addr_of_string ip, port)
  in

  let server () =
    let%lwt () = Lwt_io.printlf "This is the Caravan of Camels game log." in

    let%lwt _ =
      Lwt_io.establish_server_with_client_address server_socket_address
        client_handler
    in

    fst (Lwt.wait ())
  in

  Lwt_main.run (server ())

let run_client ip port username1 username2 =
  let client () =
    let%lwt () =
      try%lwt
        let server_socket_address =
          Unix.ADDR_INET (Unix.inet_addr_of_string ip, port)
        in

        let%lwt _, server_out = Lwt_io.open_connection server_socket_address in

        let%lwt () =
          Lwt_io.printlf
            "You are connected to the Camel Caravan game log as [%s] and [%s]."
            username1 username2
        in

        let%lwt () =
          Lwt_io.write_line server_out
            (Printf.sprintf "[%s] and [%s] have entered the chat." username1
               username2)
        in

        Lwt.return_unit
      with _ ->
        Printf.printf
          "Not connected to game log. If you want your game to be logged, ^C \
           then first connect to game log. \n";
        Lwt.return ()
    in
    Lwt.return ()
  in
  Lwt_main.run (client ())

let leaderboard_file = "leaderboard.txt"

let load_leaderboard () =
  if Sys.file_exists leaderboard_file then
    let ic = open_in leaderboard_file in
    let rec read_lines acc =
      try
        let line = input_line ic in
        read_lines (line :: acc)
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    read_lines []
  else []

let save_leaderboard leaderboard =
  let oc = open_out leaderboard_file in
  List.iter (fun entry -> Printf.fprintf oc "%s\n" entry) leaderboard;
  close_out oc

let parse_leaderboard_entry entry =
  try
    let parts = String.split_on_char ':' entry in
    let names = List.hd parts in
    let time_str = List.nth parts 1 |> String.trim in
    let time =
      float_of_string (String.sub time_str 0 (String.index time_str ' '))
    in
    (names, time)
  with _ -> ("", infinity)

let compare_entries (_, time1) (_, time2) = compare time1 time2

let format_leaderboard leaderboard =
  let sorted_leaderboard = List.sort compare_entries leaderboard in
  match sorted_leaderboard with
  | [] -> []
  | (fastest_team, fastest_time) :: rest ->
      let formatted_entries =
        List.map
          (fun (team, time) ->
            let time_behind = time -. fastest_time in
            if time_behind = 0.0 then
              Printf.sprintf "%s: %.2f seconds" team time
            else
              Printf.sprintf "%s: %.2f seconds (+%.2f seconds)" team time
                time_behind)
          rest
      in
      Printf.sprintf "%s: %.2f seconds" fastest_team fastest_time
      :: formatted_entries

let update_leaderboard username1 username2 time =
  let new_entry =
    Printf.sprintf "%s and %s: %.2f seconds" username1 username2 time
  in
  let leaderboard = load_leaderboard () in
  let updated_leaderboard = new_entry :: leaderboard in
  save_leaderboard updated_leaderboard

let print_leaderboard () =
  log_message "Leaderboard:";
  let leaderboard = load_leaderboard () in
  let parsed_leaderboard = List.map parse_leaderboard_entry leaderboard in
  let sorted_leaderboard = List.sort compare_entries parsed_leaderboard in
  let formatted_leaderboard = format_leaderboard sorted_leaderboard in
  List.iter log_message formatted_leaderboard;
  match sorted_leaderboard with
  | [] -> ()
  | (fastest_team, fastest_time) :: _ ->
      let slowest_team, slowest_time =
        List.nth sorted_leaderboard (List.length sorted_leaderboard - 1)
      in
      log_message
        (Printf.sprintf "Team with the fastest time: %s with %.2f seconds"
           fastest_team fastest_time);
      log_message
        (Printf.sprintf "Team with the slowest time: %s with %.2f seconds"
           slowest_team slowest_time)

let prompt_save_log username1 username2 time_taken =
  log_message "Do you want to save the game log? (yes/no): ";
  let response = read_line () in
  if response = "yes" then (
    let file_name = "game_log.txt" in
    let log_content =
      Printf.sprintf
        "%s and %s completed their game of Caravan of Camels with this time: \
         %.1fs\n"
        username1 username2 time_taken
    in
    let oc = open_out file_name in
    Printf.fprintf oc "%s" log_content;
    close_out oc;
    Printf.printf "Game log saved to %s\n" file_name)
  else log_message "Game log not saved."

let () =
  if Array.exists (( = ) "-h") Sys.argv then (
    print_help ();

    exit 0);
  let print_usage () =
    Printf.printf
      "Usage:\n\n\
      \                   Log: %s log <ip> <port>\n\n\
      \                   Users: %s users <ip> <port> <username1> <username2>\n"
      Sys.argv.(0) Sys.argv.(0);

    exit 1
  in

  if Array.length Sys.argv < 4 then print_usage ()
  else
    let mode = Sys.argv.(1)
    and ip = Sys.argv.(2)
    and port = int_of_string Sys.argv.(3) in

    match mode with
    | "log" -> run_server ip port
    | "users" ->
        if Array.length Sys.argv < 6 then print_usage ();
        let username1 = Sys.argv.(4) in
        let username2 = Sys.argv.(5) in

        run_client ip port username1 username2;

        let board_size = 8 in

        Random.self_init ();

        let player1 = create_new_player () in

        let x, y = place_ship player1.board 2 in

        let x1 = fst (place_ship player1.board 1) in

        let player2 = create_new_player () in

        log_message "Welcome to Camel Caravan!";

        log_message "Player 1's board:";

        let number_array = number_array_top 8 in

        print_numbers_top number_array;

        print_board player1.board;

        log_message "Player 2's board:";

        let number_array = number_array_top 8 in

        print_numbers_top number_array;

        print_board player2.board;

        let t = Unix.gettimeofday () in

        let _ =
          make_guess player1 player2 1 board_size
            (fst x, snd x)
            (fst y, snd y)
            (fst x1, snd x1)
        in

        let time_taken = Unix.gettimeofday () -. t in
        Printf.printf "You both took this long to complete the game: %.1fs\n"
          time_taken;

        prompt_save_log username1 username2 time_taken;

        update_leaderboard username1 username2 time_taken;

        print_leaderboard ()
    | _ -> print_usage ()
