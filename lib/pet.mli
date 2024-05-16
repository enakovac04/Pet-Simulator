(** Module for managing pets in a pet simulator. *)
type max_vals = {mutable health : int; mutable happiness : int; mutable energy : int} 

(** Types for representing pets and their possible configurations. *)
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

(** List of available actions or options in the pet simulator. *)
val options : string list

type animal =
  | Camel of pet
  | Dog of pet

val to_pet : animal -> pet

(** Creates a new animal with specified name and type. Throws if the type is invalid. *)
val create : string -> string -> animal

(** Converts an animal to its string representation. *)
val to_string : animal -> string
val health_to_string : animal -> string
val happiness_to_string : animal -> string
val energy_to_string : animal -> string
val nutrition_to_string : animal -> string
val money_to_string : animal -> string
val status_to_string : animal -> string

(** Health manipulation functions. *)
val increase_health : animal -> int -> unit
val decrease_health : animal -> int -> unit
val get_health : animal -> int
val max_health : animal -> int
val increase_max_health : animal -> unit

(** Happiness manipulation functions. *)
val increase_happiness : animal -> int -> unit
val decrease_happiness : animal -> int -> unit
val get_happiness : animal -> int
val max_happiness : animal -> int
val increase_max_happiness : animal -> unit

(** Energy manipulation functions. *)
val increase_energy : animal -> int -> unit
val decrease_energy : animal -> int -> unit
val get_energy : animal -> int
val max_energy : animal -> int
val increase_max_energy : animal -> unit

(** Nutrition manipulation functions. *)
val increase_nutrition : animal -> int -> unit
val decrease_nutrition : animal -> int -> unit
val get_nutrition : animal -> int

(** Money manipulation *)
val increase_money : animal -> float -> unit
val decrease_money : animal -> float -> unit
val get_money : animal -> float

(** Direct setters for pet attributes. *)
val set_health : animal -> int -> unit
val set_happiness : animal -> int -> unit
val set_energy : animal -> int -> unit
val set_nutrition : animal -> int -> unit
val set_money : animal -> float -> unit
val set_microchip : animal -> unit
val set_leash : animal -> unit
val add_sickness : animal -> string -> unit
val remove_sickness : animal -> string -> unit

val microchip_to_string : animal -> string
val get_microchip : animal -> bool
val get_leash : animal -> bool
val get_sickness : animal -> string -> bool

(** Retrieves the name of the animal. *)
val get_name : animal -> string

