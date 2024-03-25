open Cs_final

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

let create_dog () = 
  let () = print_string "What would you like to name your Dog: " in
  let name_of_dog = read_line () in
  Pet.create name_of_dog "Dog"
let display_options options = 
  let options_string = String.concat ", " options in
  Printf.printf "What would you like to do next? %s\n" options_string

let rec options () = 
  display_options Pet.options;
  let choice = read_line () in 
  match choice with
  | choice when List.mem choice Pet.options ->
    begin match choice with
    | "Feed" -> print_endline " "
    | "Walk" -> print_endline " "
    | "Play" -> print_endline " "
    | "Clean" -> print_endline " "
    | "Nap" -> print_endline " "
    | "Competition" -> print_endline " "
    | "Shop" -> print_endline " "
    | "END GAME" -> print_endline "Thank you for playing Pet Simulator. Goodbye!"; exit 0
    | _ -> ()
    end;
    options ()
  | _ -> 
    print_endline "Invalid option, please try again."; 
    options ()

let create_camel () =
  let () = print_string "What would you like to name your Camel: " in
  let name_of_camel = read_line () in
  Pet.create name_of_camel "Camel"

let rec game_select_animal () =
  let () =
    print_endline "Which animal would you like to select?\nDog  Camel"
  in
  match read_line () with
  | "Dog" -> create_dog ()
  | "Camel" -> create_camel ()
  | _ ->
      print_endline "Please choose one of the options";
      game_select_animal ()

let game_output () =
  game_start ();
  let animal = game_select_animal () in
  print_endline (Pet.to_string animal);
  options ()

let () = game_output ()