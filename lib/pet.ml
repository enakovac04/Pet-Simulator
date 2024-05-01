type pet = {
  name : string;
  mutable health : int;
  mutable happiness : int;
  mutable energy : int;
  mutable nutrition : int;
  mutable money : float;
}

let options = ["Feed"; "Walk"; "Play"; "Clean"; "Nap"; "Train"; "Competition"; "Shop"; "END GAME"]

type animal =
  | Camel of pet
  | Dog of pet

let create name animal : animal =
  let max = 10 in
  let stats = { name; health = max; happiness = max; energy = max; nutrition = max; money = 5.0} in
  match animal with
  | "Camel" -> Camel stats
  | "Dog" -> Dog stats
  | _ -> failwith "Must be valid animal"

let to_string animal = 
  match animal with
  | Camel _ ->
    "Camel"
  | Dog _ ->
    "Dog"
  
let to_pet animal =
  match animal with
  | Camel c -> c
  | Dog d -> d

let health_to_string (p : animal) : string =
  let pet = to_pet p in to_string p ^ " " ^ pet.name ^ " has " ^ string_of_int pet.health ^ "/10 health"

let happiness_to_string (p : animal) : string =
  let pet = to_pet p in to_string p ^ " " ^ pet.name ^ " has " ^ string_of_int pet.happiness ^ "/10 happiness"

let energy_to_string (p : animal) : string =
  let pet = to_pet p in to_string p ^ " " ^ pet.name ^ " has " ^ string_of_int pet.energy ^ "/10 energy"

let nutrition_to_string (p : animal) : string =
  let pet = to_pet p in to_string p ^ " " ^ pet.name ^ " has " ^ string_of_int pet.nutrition ^ "/10 nutrition"

let money_to_string (p : animal) : string =
  let pet = to_pet p in to_string p ^ " " ^ pet.name ^ " has $" ^ string_of_float pet.money

let status_to_string (p : animal) : string = 
  let pet = to_pet p in to_string p ^ " " ^ pet.name ^ " has " ^ string_of_int pet.health ^ "/10 health, " 
  ^ string_of_int pet.happiness ^ "/10 happiness, " ^ string_of_int pet.energy ^ "/10 energy, " 
  ^ string_of_int pet.nutrition ^ "/10 nutrition, and $" ^ string_of_float pet.money
  
let decrease_health animal amount = 
  match animal with
  | Dog d -> d.health <- max (d.health - amount) 0
  | Camel c -> c.health <- max (c.health - amount) 0

let increase_health animal amount = 
  match animal with
    | Dog d -> d.health <- min (d.health + amount) 10
    | Camel c -> c.health <- min (c.health + amount) 10

let get_health animal = 
  match animal with
  | Dog d -> d.health
  | Camel c -> c.health

let increase_happiness animal amount =
  match animal with
  | Dog d -> d.happiness <- min (d.happiness + amount) 10
  | Camel c -> c.happiness <- min (c.happiness + amount) 10

let decrease_happiness animal amount =
  match animal with
  | Dog d -> d.happiness <- max (d.happiness - amount) 0
  | Camel c -> c.happiness <- max (c.happiness - amount) 0

let increase_energy animal amount =
  match animal with
  | Dog d -> d.energy <- min (d.energy + amount) 10
  | Camel c -> c.energy <- min (c.energy + amount) 10

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

let set_health animal value =
  match animal with
  | Dog d -> d.health <- value
  | Camel c -> c.health <- value

let set_happiness animal value =
  match animal with
  | Dog d -> d.happiness <- value
  | Camel c -> c.happiness <- value

let set_energy animal value =
  match animal with
  | Dog d -> d.energy <- value
  | Camel c -> c.energy <- value

let set_nutrition animal value =
  match animal with
  | Dog d -> d.nutrition <- value
  | Camel c -> c.nutrition <- value

let get_name animal =
  match animal with
  | Dog d -> d.name
  | Camel c -> c.name