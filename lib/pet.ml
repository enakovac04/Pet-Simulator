type max_vals = {mutable health : int; mutable happiness : int; mutable energy : int}

type pet = {
  name : string;
  mutable health : int;
  mutable happiness : int;
  mutable energy : int;
  mutable nutrition : int;
  mutable money : float;
  mutable skills : string list;
  mutable items : string list;
  mutable sickness : string list;
  max_values : max_vals;
}

let options =
  [
    "Feed";
    "Walk";
    "Play";
    "Clean";
    "Nap";
    "Ride";
    "Vet";
    "Train";
    "Battle";
    "Chance Game";
    "Blackjack Game";
    "Cooking Game";
    "Trivia Game";
    "Shop";
    "Status";
    "Help";
    "END GAME";
  ]

type animal =
  | Camel of pet
  | Dog of pet

let create name animal : animal =
  let max = 10 in
  let stats =
    {
      name;
      health = max;
      happiness = max;
      energy = max;
      nutrition = max;
      skills = [];
      money = 5.0;
      items = [];
      sickness = [];
      max_values = {happiness = max; health = max; energy = max}
    }
  in
  match animal with
  | "Camel" -> Camel stats
  | "Dog" -> Dog stats
  | _ -> failwith "Must be valid animal"

let to_pet animal =
  match animal with
  | Camel c -> c
  | Dog d -> d

let max_health animal = 
  match animal with
  | Camel c -> (c.max_values).health
  | Dog d -> (d.max_values).health

let increase_max_health animal = 
  match animal with
  | Camel c -> (c.max_values).health <- (c.max_values).health + 1
  | Dog d -> (d.max_values).health <- (d.max_values).health + 1

let max_happiness animal = 
  match animal with
  | Camel c -> (c.max_values).happiness
  | Dog d -> (d.max_values).happiness

let increase_max_happiness animal = 
  match animal with
  | Camel c -> (c.max_values).happiness <- (c.max_values).happiness + 1
  | Dog d -> (d.max_values).happiness <- (d.max_values).happiness + 1

let max_energy animal = 
  match animal with
  | Camel c -> (c.max_values).energy
  | Dog d -> (d.max_values).energy

let increase_max_energy animal = 
  match animal with
  | Camel c -> (c.max_values).energy <- (c.max_values).energy + 1
  | Dog d -> (d.max_values).energy <- (d.max_values).energy + 1

(* TO_STRING *)
let to_string animal =
  match animal with
  | Camel _ -> "Camel"
  | Dog _ -> "Dog"

let health_to_string (p : animal) : string =
  let pet = to_pet p in
  to_string p ^ " " ^ pet.name ^ " has " ^ string_of_int pet.health
  ^ "/" ^ string_of_int (pet.max_values).health ^ " health"

let happiness_to_string (p : animal) : string =
  let pet = to_pet p in
  to_string p ^ " " ^ pet.name ^ " has "
  ^ string_of_int pet.happiness
  ^ "/" ^ string_of_int (pet.max_values).happiness ^ " happiness"

let energy_to_string (p : animal) : string =
  let pet = to_pet p in
  to_string p ^ " " ^ pet.name ^ " has " ^ string_of_int pet.energy
  ^ "/" ^ string_of_int (pet.max_values).energy ^ " energy"

let nutrition_to_string (p : animal) : string =
  let pet = to_pet p in
  to_string p ^ " " ^ pet.name ^ " has "
  ^ string_of_int pet.nutrition
  ^ "/10 nutrition"

let money_to_string (p : animal) : string =
  let pet = to_pet p in
  to_string p ^ " " ^ pet.name ^ " has $" ^ string_of_float pet.money

let status_to_string (p : animal) : string =
  let pet = to_pet p in
  to_string p ^ " " ^ pet.name ^ " has " ^ string_of_int pet.health
  ^ "/" ^ string_of_int (pet.max_values).health ^ " health, "
  ^ string_of_int pet.happiness
  ^ "/" ^ string_of_int (pet.max_values).happiness  ^ " happiness, " 
  ^ string_of_int pet.energy 
  ^ "/" ^ string_of_int (pet.max_values).energy ^ " energy, "
  ^ string_of_int pet.nutrition
  ^ "/10 nutrition, and $" ^ string_of_float pet.money

(* DECREASE/INCREASE *)  
let decrease_health animal amount =
  match animal with
  | Dog d -> d.health <- max (d.health - amount) 0
  | Camel c -> c.health <- max (c.health - amount) 0

let increase_health animal amount =
  match animal with
  | Dog d -> d.health <- min (d.health + amount) (d.max_values).health
  | Camel c -> c.health <- min (c.health + amount) (c.max_values).health

let increase_happiness animal amount =
  match animal with
  | Dog d -> d.happiness <- min (d.happiness + amount) (d.max_values).happiness
  | Camel c -> c.happiness <- min (c.happiness + amount) (c.max_values).happiness

let decrease_happiness animal amount =
  match animal with
  | Dog d -> d.happiness <- max (d.happiness - amount) 0
  | Camel c -> c.happiness <- max (c.happiness - amount) 0

let increase_energy animal amount =
  match animal with
  | Dog d -> d.energy <- min (d.energy + amount) (d.max_values).energy
  | Camel c -> c.energy <- min (c.energy + amount) (c.max_values).energy

let decrease_energy animal amount =
  match animal with
  | Dog d -> d.energy <- max (d.energy - amount) 0
  | Camel c -> c.energy <- max (c.energy - amount) 0

let increase_nutrition animal amount =
  match animal with
  | Dog d -> d.nutrition <- min (d.nutrition + amount) 10
  | Camel c -> c.nutrition <- min (c.nutrition + amount) 10

let decrease_nutrition animal amount =
  match animal with
  | Dog d -> d.nutrition <- max (d.nutrition - amount) 0
  | Camel c -> c.nutrition <- max (c.nutrition - amount) 0

let increase_money animal amount =
  match animal with
  | Dog d -> d.money <- (d.money +. amount)
  | Camel c -> c.money <- (c.money +. amount)
  
let decrease_money animal amount =
  match animal with
  | Dog d -> d.money <- max (d.money -. amount) 0.
  | Camel c -> c.money <- max (c.money -. amount) 0.

(* GET *)
let get_health animal =
  match animal with
  | Dog d -> d.health
  | Camel c -> c.health  

let get_happiness animal =
  match animal with
  | Dog d -> d.happiness
  | Camel c -> c.happiness

let get_energy animal =
  match animal with
  | Dog d -> d.energy
  | Camel c -> c.energy

let get_nutrition animal =
  match animal with
  | Dog d -> d.nutrition
  | Camel c -> c.nutrition

let get_money animal =
  match animal with
  | Dog d -> d.money
  | Camel c -> c.money

let get_name animal =
  match animal with
  | Dog d -> d.name
  | Camel c -> c.name
  
let get_microchip animal =
  match animal with
  | Dog d -> List.mem "microchip" d.items
  | Camel c -> List.mem "microchip" c.items

let get_leash animal =
  match animal with
  | Dog d -> List.mem "leash" d.items
  | Camel c -> List.mem "leash" c.items

let get_sickness animal sickness = 
  match animal with
  | Dog d -> List.mem sickness d.sickness
  | Camel c -> List.mem sickness c.sickness

(* SET *)
let set_health animal value =
  match animal with
  | Dog d -> d.health <- min (d.max_values).health value
  | Camel c -> c.health <- min (c.max_values).health value

let set_happiness animal value =
  match animal with
  | Dog d -> d.happiness <- min (d.max_values).happiness value
  | Camel c -> c.happiness <- min (c.max_values).happiness value

let set_energy animal value =
  match animal with
  | Dog d -> d.energy <- min (d.max_values).energy value
  | Camel c -> c.energy <- min (c.max_values).energy value

let set_nutrition animal value =
  match animal with
  | Dog d -> d.nutrition <- min value 10
  | Camel c -> c.nutrition <- min value 10

let set_money animal value =
  match animal with
  | Dog d -> d.money <- value
  | Camel c -> c.money <- value

let add_sickness animal sickness = 
  match animal with
  | Dog d -> if (List.mem sickness d.sickness) = false 
    then d.sickness <- sickness :: d.sickness 
  | Camel c -> if (List.mem sickness c.sickness) = false 
    then c.sickness <- sickness :: c.sickness 

let remove_sickness animal sickness = 
  match animal with
  | Dog d -> if (List.mem sickness d.sickness) 
    then d.sickness <- (List.filter (fun x -> x <> sickness) d.sickness)
  | Camel c -> if (List.mem sickness c.sickness) 
    then c.sickness <- (List.filter (fun x -> x <> sickness) c.sickness)

let set_microchip animal =
  match animal with
  | Dog d -> if (List.mem "microchip" d.items) = false 
    then d.items <- "microchip" :: d.items 
  | Camel c -> if (List.mem "microchip" c.items) = false 
    then c.items <- "microchip" :: c.items 

let set_leash animal =
  match animal with
  | Dog d -> if (List.mem "leash" d.items) = false 
    then d.items <- "leash" :: d.items 
  | Camel c -> if (List.mem "leash" c.items) = false 
    then c.items <- "leash" :: c.items 
