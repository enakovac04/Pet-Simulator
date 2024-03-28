type pet = {
  name : string;
  mutable health : int;
  mutable happiness : int;
  mutable energy : int;
  mutable nutrition : int;
  mutable money : float;
}
[@@warning "-32"]

type animal =
  | Camel of pet
  | Dog of pet

let create name animal : animal =
  (* maximum status levels *)
  let max = 10 in
  let stats = { name; health = max; happiness = max; energy = max; nutrition = max; money = 5.0} in
  match animal with
  | "Camel" -> Camel stats
  | "Dog" -> Dog stats
  | _ -> failwith "Must be valid animal"
(* When animal is "born" the health stats starts off as 10 (perfect health) and they
   start out with 5 free dollars but we can change this *)

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
  (*
  | Camel { name = n; health = h; happiness = hap; energy = e; nutrition = ntr; money = m ; _ } ->
    to_string p ^ n ^ " has " ^ string_of_int h ^ "/10 health and " 
    ^ string_of_int hap ^ "/10 happiness and " ^ string_of_int e ^ "/10 energy and " 
    ^ string_of_int ntr ^ "/10 nutrition and $" ^ string_of_float m
  | Dog { name = n; health = h; happiness = hap; energy = e; nutrition = ntr; money = m ; _ } ->
    to_string p ^ n ^ " has " ^ string_of_int h ^ "/10 health and " 
    ^ string_of_int hap ^ "/10 happiness and " ^ string_of_int e ^ "/10 energy and " 
    ^ string_of_int ntr ^ "/10 nutrition and $" ^ string_of_float m *)

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
  
let options = ["Feed"; "Walk"; "Play"; "Clean"; "Nap"; "Train"; "Competition"; "Shop"; "END GAME"]

let get_name animal =
  match animal with
  | Dog d -> d.name
  | Camel c -> c.name