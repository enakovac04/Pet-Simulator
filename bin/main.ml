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

let update_health animal amount =
  match animal with
  | Pet.Camel {name; health; money} ->
      let new_health = health + amount in
      if new_health <= 0 then 
        (print_endline "Your camel reached 0 health, and has passd away."; exit 0)
      else
        Pet.Camel {name; health = new_health; money}
  | Pet.Dog {name; health; money} ->
      let new_health = health + amount in
      if new_health <= 0 then 
        (print_endline "Your dog reached 0 health, and has passed away"; exit 0)
      else
        Pet.Dog {name; health = new_health; money}


let rec feed animal = 
  let name = Pet.get_name animal in  
  Printf.printf "Choose what food to feed %s: Chocolate Grapes Cheese Pork Fish\n" name;
  let food = read_line () in
  match food, animal with
  | "Chocolate", Pet.Dog _ -> print_endline "Your dog ate chocolate and got sick. Minus 2 health."; update_health animal (-2)
  | "Chocolate", _ -> print_endline "Luckily, your pet can eat chocolate safely."; animal
  | "Grapes", Pet.Dog _ -> print_endline "Your dog ate grapes and got sick. Minus 2 health."; update_health animal (-2)
  | "Grapes", _ -> print_endline "Luckily, your pet can eat grapes safely."; animal
  | "Cheese", _ -> print_endline "Your pet enjoyed the cheese."; animal
  | "Pork", _ -> print_endline "Your pet enjoyed the pork."; animal
  | "Fish", _ -> print_endline "Your pet enjoyed the fish."; animal
  | _, _ -> print_endline "Not one of the options"; feed animal

let rec options animal = 
  display_options Pet.options;
  let choice = read_line () in 
  match choice with
  | choice when List.mem choice Pet.options ->
    begin match choice with
    | "Feed" -> let updated_animal = feed animal in options updated_animal
    | "Walk" -> options animal
    | "Play" -> options animal
    | "Clean" -> options animal
    | "Nap" -> options animal
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