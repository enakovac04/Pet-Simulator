let rec game_start () =
  let () = print_string "Would you like to enter Pet Simulator? Y/N: " in
  match read_line () with
  | "Y" -> print_endline "Welcome to Pet Simulator!"
  | "N" ->
      print_endline "Please play the game!";
      game_start ()
  | _ ->
      print_endline "Please enter Y or N.";
      game_start ()

let rec game_select_animal () =
  let () =
    print_endline "Which animal would you like to select?\nDog  Cat  Camel"
  in
  match read_line () with
  | "Dog" -> print_endline "Dog"
  | "Cat" -> print_endline "Cat"
  | "Camel" -> print_endline "Camel"
  | _ ->
      print_endline "Please choose one of the options";
      game_select_animal ()

let game_output () =
  game_start ();
  game_select_animal ()

let () =
  game_output ();
  print_endline "Hello"
