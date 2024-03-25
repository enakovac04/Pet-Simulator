let rec game_start () =
  let () = print_string "Would you like to enter Pet Simulator? Y/N: " in
  match read_line () with
  | "Y" -> print_endline "Welcome to Pet Simulator!"
  | "N" -> print_endline "Please play the game!"; game_start ()
  | _ -> print_endline "Please enter Y or N."; game_start ()

let game_output () = 
  game_start()

let () = game_output ()