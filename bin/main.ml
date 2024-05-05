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

let create_camel () =
  let () = print_string "What would you like to name your Camel: " in
  let name_of_camel = read_line () in
  Pet.create name_of_camel "Camel"

let display_options options = 
  let options_string = String.concat ", " options in
  Printf.printf "What would you like to do next? %s\n" options_string

let decrease_health animal amount =
  Pet.decrease_health animal amount; 
  if Pet.get_health animal = 0 then (print_endline "Your pet reached 0 health, and has gitpassed away."; exit 0)
  else Printf.printf "-%i health \n" amount

let increase_health animal amount =
  Pet.increase_health animal amount; 
   Printf.printf "+%i health \n" amount

let increase_happiness animal amount =
  Pet.increase_happiness animal amount; 
  Printf.printf "+%i happiness \n" amount

let decrease_happiness animal amount =
  Pet.decrease_happiness animal amount; 
  Printf.printf "-%i happiness \n" amount

let increase_energy animal amount =
  Pet.increase_energy animal amount; 
  Printf.printf "+%i energy \n" amount

let decrease_energy animal amount =
  Pet.decrease_energy animal amount; 
  Printf.printf "-%i energy \n" amount

let increase_nutrition animal amount =
  Pet.increase_nutrition animal amount; 
  Printf.printf "+%i nutrition \n" amount

let decrease_nutrition animal amount =
  Pet.decrease_nutrition animal amount; 
  Printf.printf "-%i nutrition \n" amount

let rec feed animal = 
  let name = Pet.get_name animal in  
  Printf.printf "Choose what food to feed %s: Chocolate Grapes Cheese Pork Fish\n" name;
  let food = read_line () in
  match food, animal with
  | "Chocolate", Pet.Dog _ -> 
    Printf.printf "Your dog %s ate chocolate and got sick.\n" name; 
    decrease_nutrition animal 2; 
    decrease_health animal 2
  | "Chocolate", _ ->
    Printf.printf "Luckily, your pet %s can eat chocolate safely.\n" name;
  | "Grapes", Pet.Dog _ -> 
    Printf.printf "Your dog %s ate grapes and got sick.\n" name; 
    decrease_nutrition animal 2; 
    decrease_health animal 2
  | "Grapes", _ -> 
    Printf.printf "Luckily, your pet %s can eat grapes safely. \n" name; 
  | "Cheese", _ -> 
    Printf.printf "%s enjoyed the cheese. \n" name; 
    increase_nutrition animal 2; 
    increase_health animal 1
  | "Pork", _ -> 
    Printf.printf "%s enjoyed the pork. \n" name; 
    increase_nutrition animal 2; 
    increase_health animal 1
  | "Fish", _ -> 
    Printf.printf "%s enjoyed the fish. \n" name; 
    increase_nutrition animal 2; 
    increase_health animal 1
  | _, _ -> print_endline "Not one of the options\n"; 
  feed animal

let rec play animal = 
  let name = Pet.get_name animal in
  Printf.printf "Choose a toy to play with %s: Ball Rope Bone Trash\n" name;
  let toy = read_line () in 
  match toy, animal with
  | "Ball", _ -> 
    Printf.printf "%s loved the toy! \n" name; 
    increase_happiness animal 2; decrease_energy animal 1
  | "Rope", _ -> 
    Printf.printf "%s loved the toy! \n" name; 
    increase_happiness animal 2;
    decrease_energy animal 1
  | "Bone", Pet.Dog _ -> 
    Printf.printf "%s loved the toy! \n" name; 
    increase_happiness animal 2;
    decrease_energy animal 1
  | "Bone", _ -> 
    Printf.printf "%s did not like the toy. \n" name; 
    decrease_happiness animal 2
  | "Trash", _ ->  
    Printf.printf "%s did not like the toy. \n" name; 
    decrease_happiness animal 2
  | _, _ -> print_endline "Not one of the options \n"; 
  play animal

let rec walk animal =
  let name = Pet.get_name animal in  
  Printf.printf "Where would you like to walk %s? Options: Park, Street, Desert, Swim\n" name;
  let location = read_line () in
  match location, animal with
  | "Park", Pet.Dog _ ->
      Printf.printf "%s enjoyed a lovely walk in the park.\n" name;
      increase_happiness animal 2;
      decrease_energy animal 1
  | "Park", Pet.Camel _ ->
      Printf.printf "You got strange looks for bringing a %s to a park. \n" name;
      decrease_energy animal 2
  | "Street", _ ->
      Printf.printf "%s had a nice time walking on the street.\n" name;
      increase_happiness animal 1;
      decrease_energy animal 1
  | "Desert", Pet.Dog _ ->
      Printf.printf "%s found the desert walk challenging and feels tired.\n" name;
      decrease_health animal 2;
      decrease_energy animal 2
  | "Desert", Pet.Camel _ ->
      Printf.printf "%s found the desert walk refreshing and feels energized.\n" name;
      Pet.decrease_health animal 2;
      Pet.increase_energy animal 2
  | "Swim", Pet.Dog _ ->
      Printf.printf "%s had a great time swimming!\n" name;
      Pet.increase_happiness animal 3;
      Pet.decrease_energy animal 2;
      Pet.increase_health animal 1
  | "Swim", Pet.Camel _ ->
      Printf.printf "%s does not know how to swim...\n" name;
      Pet.increase_happiness animal 3;
      Pet.decrease_energy animal 2;
      Pet.decrease_health animal 3
  | _, _ ->
      Printf.printf "That's not a valid walking option. Please choose again.\n";
      walk animal

let rec clean animal =
  let name = Pet.get_name animal in
  Printf.printf "How would you like to clean %s? Options: Bath, Dry Shampoo, Brush\n" name;
  let methods = read_line () in
  match methods, animal with
  | "Bath", Pet.Dog _ ->
      Printf.printf "%s enjoyed the bath and feels refreshed!\n" name;
      increase_happiness animal 2;
      increase_health animal 1
  | "Bath", Pet.Camel _ ->
      Printf.printf "Bathing %s was challenging but necessary.\n" name;
      decrease_happiness animal 1;
      increase_health animal 1
  | "Dry Shampoo", Pet.Dog _ ->
      Printf.printf "%s feels cleaner with dry shampoo.\n" name;
      increase_happiness animal 1
  | "Dry Shampoo", Pet.Camel _ ->
      Printf.printf "%s appreciates the quick clean!\n" name;
      increase_happiness animal 2
  | "Brush", _ ->
      Printf.printf "%s loves being brushed and looks great!\n" name;
      increase_happiness animal 1
  | _, _ ->
      Printf.printf "That's not a valid cleaning option. Please choose again.\n";
      clean animal

let rec options animal = 
  display_options Pet.options;
  let choice = read_line () in 
  match choice with
  | choice when List.mem choice Pet.options ->
    begin match choice with
    | "Feed" -> feed animal; 
    options animal
    | "Walk" -> walk animal; 
    options animal
    | "Play" ->  play animal
    | "Clean" -> clean animal;
    options animal
    | "Nap" -> options animal
    | "Train" -> options animal
    | "Competition" -> options animal
    | "Shop" -> options animal
    | "END GAME" -> print_endline "Thank you for playing Pet Simulator. Goodbye!"; 
    exit 0
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