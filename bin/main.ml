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
  let health = Pet.get_health animal in
  let name = Pet.get_name animal in
  if health = 0 then 
    (Printf.printf "%s reached 0 health, and has passed away." name; exit 0)
  else if health < 3 then 
    Printf.printf "%s has poor health and has been diagnosed with a fever.\n" name
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

let status animal =
  Printf.printf "%s \n" (Pet.status_to_string animal)

let rec feed animal = 
  let name = Pet.get_name animal in  
  Printf.printf "Choose what food to feed %s: Chocolate, Grapes, Cheese, Pork, Fish\n" name;
  let food = read_line () in
  match food, animal with
  | "Chocolate", Pet.Dog _ -> 
    Printf.printf "Your dog %s ate chocolate and threw upk.\n" name; 
    decrease_nutrition animal 2; 
    decrease_health animal 2
  | "Chocolate", _ ->
    Printf.printf "Luckily, your pet %s can eat chocolate safely.\n" name;
  | "Grapes", Pet.Dog _ -> 
    Printf.printf "Your dog %s ate grapes and threw up.\n" name; 
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
    increase_happiness animal 2;
    decrease_energy animal 1
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

let ran_away animal leash = 
  let name = Pet.get_name animal in
  if leash then () 
  else (Printf.printf "%s ran away!\n" name; 
    let microchip = Pet.get_microchip animal in
      if microchip then 
        Printf.printf "Luckily, %s has a microchip so you were able to find your pet! \n" name
      else (Printf.printf "You could not find %s." name; exit 0)
  )

let rec walk animal =
  let name = Pet.get_name animal in  
  Printf.printf "Would you like to bring a leash? Y/N \n";
  let response = read_line () in
  Printf.printf "Where would you like to walk %s? Options: Park, Street, Desert, Swim\n" name;
  let location = read_line () in
  match location, animal with
  | "Park", Pet.Dog _ ->
    if response = "Y" then ran_away animal true else ran_away animal false;
    Printf.printf "%s enjoyed a lovely walk in the park.\n" name;
    increase_happiness animal 2;
    decrease_energy animal 1;
    increase_health animal 1
  | "Park", Pet.Camel _ ->
    if response = "Y" then ran_away animal true else ran_away animal false;
    Printf.printf "You got strange looks for bringing a %s to a park. \n" name;
    decrease_energy animal 2
  | "Street", _ ->
    if response = "Y" then ran_away animal true else ran_away animal false;
    Printf.printf "%s had a nice time walking on the street.\n" name;
    increase_happiness animal 1;
    decrease_energy animal 1;
    increase_health animal 1
  | "Desert", Pet.Dog _ ->
    if response = "Y" then ran_away animal true else ran_away animal false;
    Printf.printf "%s found the desert walk challenging and feels tired.\n" name;
    decrease_energy animal 2;
    decrease_health animal 2
  | "Desert", Pet.Camel _ ->
    if response = "Y" then ran_away animal true else ran_away animal false;
    Printf.printf "%s found the desert walk refreshing and feels energized.\n" name;
    increase_energy animal 2;
    decrease_health animal 2
  | "Swim", Pet.Dog _ ->
    Printf.printf "%s had a great time swimming!\n" name;
    increase_happiness animal 3;
    decrease_energy animal 2;
    increase_health animal 1
  | "Swim", Pet.Camel _ ->
    Printf.printf "%s does not know how to swim...\n" name;
    increase_happiness animal 3;
    decrease_energy animal 2;
    decrease_health animal 3
  | _, _ ->
      Printf.printf "That's not a valid walking option. Please choose again.\n";
      walk animal

let rec clean animal =
  let name = Pet.get_name animal in
  Printf.printf "How would you like to clean %s? Options: Bath, Dry Shampoo, Brush, Mud\n" name;
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
  | "Mud", _ ->
    Printf.printf "%s is dirty and got fleas! \n" name;
    decrease_health animal 2
  | _, _ ->
      Printf.printf "That's not a valid cleaning option. Please choose again.\n";
      clean animal

let rec nap animal =
  let name = Pet.get_name animal in
  Printf.printf "Where would you like %s to nap? Options: Bed, Sand, Box, Concrete \n" name;
  let methods = read_line () in
  match methods, animal with
    | "Bed", _ -> 
      Printf.printf "%s was very comfortable and slept well!\n" name;
      increase_happiness animal 1;
      increase_energy animal 2;
      increase_health animal 1
    | "Sand", Pet.Camel _ -> 
      Printf.printf "%s was very comfortable and slept well!\n" name;
      increase_happiness animal 1;
      increase_energy animal 2;
      increase_health animal 1
    | "Sand", _ -> 
      Printf.printf "%s slept fine.\n" name;
     increase_energy animal 1
    | "Box", Pet.Camel _ ->
      Printf.printf "%s could not fit!" name;
      decrease_happiness animal 2;
      decrease_energy animal 2;
      decrease_health animal 1
    | "Box", _ -> 
      Printf.printf "%s slept fine.\n" name;
      increase_energy animal 1
    | "Concrete", _ -> 
      Printf.printf "%s was very uncomfortable and could not sleep!\n" name;
      decrease_happiness animal 2;
      decrease_energy animal 2;
    | _, _ ->
      Printf.printf "That's not a valid nap option. Please choose again.\n";
      nap animal

let rec navigate_training options animal skills =
  let rec ask_questions stages points_accumulated =
    match stages with
    | [] ->
      Printf.printf "Training session finished! Evaluating your pet's improvements...\n";
      finalize_training animal points_accumulated skills
    | (question, correct_answer) :: tail ->
      Printf.printf "%s\n" question;
      let answer = read_line () in
      if String.lowercase_ascii answer = String.lowercase_ascii correct_answer then
        begin
          Printf.printf "Excellent choice! Moving forward.\n";
          ask_questions tail (points_accumulated + 1)
        end
      else
        begin
          Printf.printf "Incorrect choice. Try to adjust your strategies.\n";
          ask_questions tail points_accumulated
        end
  in
  ask_questions options 0

and finalize_training animal points_accumulated skills =
  let pet = Pet.to_pet animal in
  if points_accumulated >= 8 then
    let available_skills = List.filter (fun skill -> not (List.mem skill pet.skills)) skills in
    if available_skills <> [] then
      let skill_earned = List.nth available_skills (Random.int (List.length available_skills)) in
      pet.skills <- skill_earned :: pet.skills; 
      Printf.printf "Congratulations! Your pet has learned a new skill: %s!\n" skill_earned;
      Pet.increase_health animal 1;
      Pet.increase_happiness animal 1;
    else
      Printf.printf "Your pet has already mastered all available skills.\n"
  else if points_accumulated > 0 then
    Printf.printf "Good effort, but more training is needed to master a new skill.\n"
  else
    Printf.printf "No significant progress this session. Try different strategies next time.\n"

let speed_training animal =
  let speed_options = [
    ("Choose the best method for speed: Treadmill, Sprinting, Swimming", "Sprinting");
    ("Set the pace: Fast, Moderate, Slow", "Fast");
    ("Choose duration: 30 minutes, 1 hour, 2 hours", "1 hour");
    ("Pick recovery method: Stretching, Massage, Rest", "Stretching");
    ("Select the environment: Indoor track, Outdoor track, Gym", "Outdoor track");
    ("Decide on the coach's style: Encouraging, Strict, Technical", "Technical");
    ("Pick a warm-up activity: Jumping jacks, Dynamic stretching, None", "Dynamic stretching");
    ("Choose your cooling down routine: Slow jogging, Yoga, Immediate rest", "Slow jogging")
  ] in
  let skills = ["Agility"; "Stamina"; "Quick Reflexes"; "Endurance"] in
  navigate_training speed_options animal skills

let strength_training animal =
  let strength_options = [
    ("Select the right weight training: Free weights, Machine weights, Body weight", "Free weights");
    ("Pick the intensity: High, Medium, Low", "High");
    ("Set training frequency: Daily, Every other day, Weekly", "Every other day");
    ("Choose a recovery plan: Protein supplements, Carbs load, Hydration", "Protein supplements");
    ("Select the training venue: Gym, Home, Outdoor park", "Gym");
    ("Determine the training time: Morning, Afternoon, Evening", "Morning");
    ("Decide on a spotter: Yes, No", "Yes");
    ("Pick motivational music: Rock, Pop, None", "Rock")
  ] in
  let skills = 
    ["Muscle Gain"; "Strength"; "Toughness"; "Power"] in
  navigate_training strength_options animal skills

let intelligence_training animal =
  let intelligence_options = [
    ("Choose the cognitive focus: Memory, Problem-solving, Logical thinking", "Problem-solving");
    ("Select a puzzle type: Rubik's cube, Sudoku, Crosswords", "Sudoku");
    ("Set the complexity: Easy, Intermediate, Advanced", "Advanced");
    ("Decide on session length: 15 minutes, 30 minutes, 1 hour", "30 minutes");
    ("Choose feedback type: After each attempt, End of session, No feedback", "After each attempt");
    ("Select training location: Quiet room, Library, Public park", "Library");
    ("Pick a rest interval: 5 min, 10 min, No rest", "5 min");
    ("Choose interaction level: Solo, Group, With trainer", "With trainer")
  ] in
  let skills = ["Quick Thinking"; "Mental Agility"; "Focus"; "Strategic Planning"] in
  navigate_training intelligence_options animal skills

let rec train animal =
  Printf.printf "Advanced Training Session: Choose a focus for today's training - Speed, Strength, Intelligence.\n";
  let focus = String.lowercase_ascii (read_line ()) in
  match focus with
  | "speed" -> speed_training animal
  | "strength" -> strength_training animal
  | "intelligence" -> intelligence_training animal
  | _ ->
    Printf.printf "Invalid focus. Please select a valid focus for training.\n";
    train animal

type shop_item = {
  name : string;
  effect : string;
  change : int;
  cost : float;
}

let shop_items = [
  {name = "Vitamin Boost"; effect = "health"; change = 2; cost = 2.5};
  {name = "Energy Drink"; effect = "energy"; change = 3; cost = 3.0};
  {name = "Happiness Cookie"; effect = "happiness"; change = 2; cost = 1.5};
  {name = "Nutrition Snack"; effect = "nutrition"; change = 1; cost = 2.0};
]

let display_shop_items () =
  Printf.printf "Available items for purchase:\n";
  List.iteri (fun i item ->
    Printf.printf "%d: %s - %s +%d (Cost: $%.2f)\n" (i+1) item.name item.effect item.change item.cost
  ) shop_items;
  Printf.printf "Enter the number to buy an item or 'exit' to leave: "

let rec shop animal =
  display_shop_items ();
  let input = read_line () in
  match input with
  | "exit" -> ()
  | _ -> process_input input animal

and process_input input animal =
  match int_of_string_opt input with
  | Some index when index > 0 && index <= List.length shop_items ->
      let item = List.nth shop_items (index - 1) in
      confirm_purchase item animal
  | _ ->
      Printf.printf "Invalid input. Please enter a valid number or 'exit'.\n";
      shop animal

and confirm_purchase item animal =
  Printf.printf "Confirm purchase of %s for $%.2f? (yes/no): " item.name item.cost;
  match String.trim (String.lowercase_ascii (read_line ())) with
  | "yes" -> process_transaction item animal
  | "no" -> shop animal
  | _ ->
      Printf.printf "Please type 'yes' or 'no' to confirm.\n";
      confirm_purchase item animal

and process_transaction item animal =
  let pet = Pet.to_pet animal in
  if pet.money >= item.cost then
    begin
      pet.money <- pet.money -. item.cost;
      apply_effects item animal;
      Printf.printf "You have purchased %s. Remaining Balance: $%.2f\n" item.name pet.money;
      shop animal
    end
  else
    begin
      Printf.printf "Not enough money. Your balance: $%.2f\n" pet.money;
      shop animal
    end

and apply_effects item animal =
  match item.effect with
  | "health" -> Pet.increase_health animal item.change
  | "happiness" -> Pet.increase_happiness animal item.change
  | "energy" -> Pet.increase_energy animal item.change
  | "nutrition" -> Pet.increase_nutrition animal item.change
  | _ -> ()

let rec battle animal =
  Printf.printf "Welcome to the Pet Battle! Your performance will determine your prize.\n";
  let stages = [
    ("Choose the right gear: Light Armor, Heavy Armor, No Armor", "Light Armor", 10);
    ("Select the correct starting position: Front, Middle, Back", "Back", 5);  
    ("Choose your strategy: Aggressive, Defensive, Balanced", "Balanced", 15);
    ("Pick the right rest interval: 1 min, 2 min, 5 min", "2 min", 10);  
    ("Select your final move: Charge, Evade, Counterattack", "Charge", 20); 
    ("Choose your victory pose: Jump, Roll, Sit", "Sit", 5)
  ] in
  let rec navigate_stages current_stages animal points_accumulated =
    match current_stages with
    | [] -> 
      Printf.printf "Battle finished! Calculating results based on your score of %d points...\n" points_accumulated;
      reward animal points_accumulated
    | (question, correct_answer, point_value) :: tail ->
      Printf.printf "%s\n" question;
      let answer = read_line () in
      if String.lowercase_ascii answer = String.lowercase_ascii correct_answer then
        begin
          Printf.printf "Correct choice! + %d points. Moving to the next challenge.\n" point_value;
          navigate_stages tail animal (points_accumulated + point_value)
        end
      else
        begin
          Printf.printf "Wrong choice! No points. Try again or type 'exit' to leave the battle.\n";
          if String.lowercase_ascii (read_line ()) = "exit" then ()
          else navigate_stages current_stages animal points_accumulated
        end
  in
  navigate_stages stages animal 0

and reward animal points_accumulated =
  let pet = Pet.to_pet animal in
  let base_prize = 2.0 in
  let multiplier = float_of_int points_accumulated *. 0.1 in
  let total_prize = base_prize +. multiplier in
  Printf.printf "Congratulations! You have won $%.2f based on your performance!\n" total_prize;
  pet.money <- pet.money +. total_prize;
  Printf.printf "Your new balance is $%.2f\n" pet.money

let rec vet animal = 
  let name = Pet.get_name animal in
  Printf.printf "What would you like to treat %s for? Microchip, Fleas, Puking, Fever, Depression\n" name;
  let choice = read_line () in 
  match choice with
  | "Microchip" -> (
    match Pet.get_microchip animal with
    | true -> Printf.printf "%s already has a microchip.\n" name
    | false ->  Printf.printf "%s has successfully recieved a microchip.\n" name;
    Pet.set_microchip animal
  )
  | "Fleas" -> 
    Printf.printf "The vet gave %s flea shampoo. %s is all better!\n" name name;
    increase_health animal 3
  | "Puking" -> 
    Printf.printf "The vet gave %s probiotics. %s feels better!\n" name name;
    increase_health animal 3;
    increase_nutrition animal 2
  | "Fever" -> 
    Printf.printf "The vet gave %s medicine. %s feels better!\n" name name;
    increase_health animal 3;
  | "Depression" ->
    Printf.printf "The vet gave %s antidepressants. %s feel better!\n" name name;
    increase_health animal 3;
    increase_happiness animal 3;
    increase_energy animal 2
  | _ ->
      Printf.printf "That's not a valid option. Please choose again.\n";
      vet animal

let rec options animal = 
  display_options Pet.options;
  let choice = read_line () in 
  match choice with
  | choice when List.mem choice Pet.options ->
    begin match choice with
    | "Feed" -> 
      feed animal; 
      status animal; 
      options animal
    | "Walk" -> 
      walk animal; 
      status animal; 
      options animal
    | "Play" -> 
      play animal; 
      status animal; 
      options animal
    | "Clean" -> 
      clean animal; 
      status animal; 
      options animal
    | "Nap" -> 
      nap animal; 
      status animal; 
      options animal
    | "Train" ->  
      train animal;
      options animal
    | "Battle" -> 
      battle animal;
      options animal
    | "Shop" ->
      shop animal;
      status animal;
      options animal
    | "Status" -> 
      status animal;
      options animal
    | "Groom" -> options animal;
    | "Event" -> options animal;
    | "Job" -> options animal;
    | "Explore" -> options animal;
    | "Vet" -> 
      vet animal;
      status animal;
      options animal;
    | "Socializing" -> options animal;
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