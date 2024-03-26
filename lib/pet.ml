type pet = {
  name : string;
  health : int;
  money : float;
}
[@@warning "-32"]

type animal =
  | Camel of pet
  | Dog of pet

let create name animal : animal =
  match animal with
  | "Camel" -> Camel { name; health = 10; money = 5.0 }
  | "Dog" -> Dog { name; health = 10; money = 5.0 }
  | _ -> failwith "Must be valid animal"
(* When animal is "born" the health starts of as 10 (perfect health) and they
   start out with 5 free dollars but we can change this *)

let status_to_string (p : animal) : string =
  match p with
  | Camel { name = n; health = h; money = m } ->
      "Camel " ^ n ^ " has " ^ string_of_int h ^ "/10 health and $"
      ^ string_of_float m
  | Dog { name = n; health = h; money = m } ->
      "Dog " ^ n ^ " has " ^ string_of_int h ^ "/10 health and $"
      ^ string_of_float m

let options = ["Feed"; "Walk"; "Play"; "Clean"; "Nap"; "Competition"; "Shop"; "END GAME"]

let get_name animal =
  match animal with
  | Dog {name; _} | Camel {name; _} -> name

let to_string animal = 
  match animal with
  | Camel { name = _; health = _; money = _} ->
    "Camel"
| Dog { name = _; health = _; money = _ } ->
    "Dog"