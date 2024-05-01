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

let create_camel () =
  let () = print_string "What would you like to name your Camel: " in
  let name_of_camel = read_line () in
  Pet.create name_of_camel "Camel"

let decrease_health animal amount =
  Pet.decrease_health animal amount; 
  if Pet.get_health animal = 0 then (print_endline "Your pet reached 0 health, and has gitpassed away."; exit 0)
(*
  match animal with
  | Pet.Camel {health; money} ->
      let new_health = health - amount in
      if new_health <= 0 then 
        (print_endline "Your camel reached 0 health, and has passd away."; exit 0)
      else
        Pet.Camel {name; health = new_health; money}
  | Pet.Dog {name; health; money} ->
      let new_health = health + amount in
      if new_health <= 0 then 
        (print_endline "Your dog reached 0 health, and has passed away"; exit 0)
      else
        Pet.Dog {name; health = new_health; money}*)
(*
let increase_health animal amount = Pet.increase_health animal amount *)

let rec feed animal = 
  let name = Pet.get_name animal in  
  Printf.printf "Choose what food to feed %s: Chocolate Grapes Cheese Pork Fish\n" name;
  let food = read_line () in
  match food, animal with
  | "Chocolate", Pet.Dog _ -> print_endline "Your dog ate chocolate and got sick. Minus 2 health."; decrease_health animal 2
  | "Chocolate", _ -> print_endline "Luckily, your pet can eat chocolate safely.";
  | "Grapes", Pet.Dog _ -> print_endline "Your dog ate grapes and got sick. Minus 2 health."; decrease_health animal 2
  | "Grapes", _ -> print_endline "Luckily, your pet can eat grapes safely."; 
  | "Cheese", _ -> print_endline (name ^ " enjoyed the cheese."); 
  | "Pork", _ -> print_endline (name ^ " enjoyed the pork."); 
  | "Fish", _ -> print_endline (name ^ " enjoyed the fish."); 
  | _, _ -> print_endline "Not one of the options"; feed animal

let rec play animal = 
  let name = Pet.get_name animal in
  Printf.printf "Choose a toy to play with %s: Ball Rope Bone Trash\n" name;
  let toy = read_line () in 
  match toy, animal with
  | "Ball", _ -> print_endline (name ^ " loved the toy!"); animal
  | "Rope", _ -> print_endline (name ^ " loved the toy!"); animal
  | "Bone", Pet.Dog _ -> print_endline (name ^ " loved the toy!"); animal
  | "Bone", _ -> print_endline (name ^ " did not like the toy"); animal
  | "Trash", _ -> print_endline (name ^ " did not like the toy"); animal
  | _, _ -> print_endline "Not one of the options"; play animal

let rec walk animal =
  let name = Pet.get_name animal in  
  Printf.printf "Where would you like to walk %s? Options: Park, Street, Desert, Swim\n" name;
  let location = read_line () in
  match location, animal with
  | "Park", Pet.Dog _ ->
      Pet.increase_happiness animal 2;
      Pet.decrease_energy animal 1;
      Printf.printf "%s enjoyed a lovely walk in the park.\n" name;
  | "Park", Pet.Camel _ ->
      Pet.decrease_energy animal 2;
      Printf.printf "You got strange looks for bringing a %s to a park. \n" name;
  | "Street", _ ->
      Pet.increase_happiness animal 1;
      Pet.decrease_energy animal 1;
      Printf.printf "%s had a nice time walking on the street.\n" name;
  | "Desert", Pet.Dog _ ->
      Pet.decrease_health animal 2;
      Pet.decrease_energy animal 2;
      Printf.printf "%s found the desert walk challenging and feels tired.\n" name;
  | "Desert", Pet.Camel _ ->
      Pet.decrease_health animal 2;
      Pet.increase_energy animal 2;
      Printf.printf "%s found the desert walk refreshing and feels energized.\n" name;
  | "Swim", Pet.Dog _ ->
      Pet.increase_happiness animal 3;
      Pet.decrease_energy animal 2;
      Pet.increase_health animal 1;
      Printf.printf "%s had a great time swimming!\n" name;
  | "Swim", Pet.Camel _ ->
      Pet.increase_happiness animal 3;
      Pet.decrease_energy animal 2;
      Pet.decrease_health animal 3;
      Printf.printf "%s does not know how to swim...\n" name;
  | _, _ ->
      Printf.printf "That's not a valid walking option. Please choose again.\n";
      walk animal

let rec options animal = 
  display_options Pet.options;
  let choice = read_line () in 
  match choice with
  | choice when List.mem choice Pet.options ->
    begin match choice with
    | "Feed" -> feed animal; options animal
    | "Walk" -> walk animal; options animal
    | "Play" -> let updated_animal = play animal in options updated_animal
    | "Clean" -> options animal
    | "Nap" -> options animal
    | "Train" -> options animal
    | "Competition" -> options animal
    | "Shop" -> options animal
    | "END GAME" -> print_endline "Thank you for playing Pet Simulator. Goodbye!"; exit 0
    | _ -> ()
    end;
  | _ -> 
    print_endline "Invalid option, please try again."; 
    options animal

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
  print_endline (Pet.status_to_string animal);
  options animal

let () = game_output ()