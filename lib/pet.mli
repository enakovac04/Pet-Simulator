(** Module for managing pets in a pet simulator. *)

(** type [max_vals] stores the maximum values for 
    health, happiness, and energy. *)
type max_vals = {mutable health : int; mutable happiness : int; mutable energy : int} 

(** type [pet] represents pets and their possible configurations. *)
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

(** [options] Lists the available actions or options in the pet simulator. *)
val options : string list

(** type [animal] represents the species for a pet. *)
type animal =
  | Camel of pet
  | Dog of pet

(** [to_pet] returns an animal as a pet type. *)
val to_pet : animal -> pet

(** [create] creates a new animal with the specified name and type. 
    Throws if the type is invalid. *)
val create : string -> string -> animal

(** [to_string] returns the given animal as its string representation. *)
val to_string : animal -> string

(** [health_to_string] returns the health level of the given [animal] in 
    its string representation. *)
val health_to_string : animal -> string

(** [happiness_to_string] returns the happiness level of the given [animal] in 
    its string representation. *)
val happiness_to_string : animal -> string

(** [energy_to_string] returns the energy level of the given [animal] in 
    its string representation. *)
val energy_to_string : animal -> string

(** [nutrition_to_string] returns the nutrition level of the given [animal] in 
    its string representation. *)
val nutrition_to_string : animal -> string

(** [money_to_string] returns the money balance of the given [animal] in 
    its string representation. *)
val money_to_string : animal -> string

(** [status_to_string] returns the status level of the given [animal] in 
  its string representation.
Displays the animal's level of health, happiness, energy, nutrition, and money. *)
val status_to_string : animal -> string

(** Health manipulation functions. *)
(** [increase_health] increases the health of the given [animal] by [amount]. *)
val increase_health : animal -> int -> unit

(** [decrease_health] decreases the health of the given [animal] by [amount]. *)
val decrease_health : animal -> int -> unit

(** [get_health] returns the amount of health the given [animal] has. *)
val get_health : animal -> int

(** [max_health] returns the maximum level of health 
    the given [animal] can have. *)
val max_health : animal -> int

(** [increase_max_health] increases the maximum level of health 
    the given [animal] can have by 1. *)
val increase_max_health : animal -> unit

(** Happiness manipulation functions. *)
(** [increase_happiness] increases the happiness of the given [animal] by [amount]. *)
val increase_happiness : animal -> int -> unit

(** [decrease_happiness] decreases the happiness of the given [animal] by [amount]. *)
val decrease_happiness : animal -> int -> unit

(** [get_happiness] returns the amount of happiness the given [animal] has. *)
val get_happiness : animal -> int

(** [max_happiness] returns the maximum level of happiness
    the given [animal] can have. *)
val max_happiness : animal -> int

(** [increase_max_happiness] increases the maximum level of happiness
    the given [animal] can have by 1. *)
val increase_max_happiness : animal -> unit

(** Energy manipulation functions. *)
(** [increase_energy] increases the energy of the given [animal] by [amount]. *)
val increase_energy : animal -> int -> unit

(** [decrease_energy] decreases the energy of the given [animal] by [amount]. *)
val decrease_energy : animal -> int -> unit

(** [get_energy] returns the amount of energy the given [animal] has. *)
val get_energy : animal -> int

(** [max_energy] returns the maximum level of energy
    the given [animal] can have. *)
val max_energy : animal -> int

(** [increase_max_energy] increases the maximum level of energy
    the given [animal] can have by 1. *)
val increase_max_energy : animal -> unit

(** Nutrition manipulation functions. *)
(** [increase_nutrition] increases the nutrition of the given [animal] by [amount]. *)
val increase_nutrition : animal -> int -> unit

(** [decrease_nutrition] decreases the nutrition of the given [animal] by [amount]. *)
val decrease_nutrition : animal -> int -> unit

(** [get_nutrition] returns the amount of nutrition the given [animal] has. *)
val get_nutrition : animal -> int

(** Money manipulation *)
(** [increase_money] increases the money of the given [animal] by [amount]. *)
val increase_money : animal -> float -> unit

(** [decrease_money] decreases the money of the given [animal] by [amount]. *)
val decrease_money : animal -> float -> unit

(** [get_money] returns the amount of money the given [animal] has. *)
val get_money : animal -> float

(** Direct setters for pet attributes. *)
(** [set_health] sets the health level of the given [animal]. *)
val set_health : animal -> int -> unit

(** [set_happiness] sets the happiness level of the given [animal]. *)
val set_happiness : animal -> int -> unit

(** [set_energy] sets the energy level of the given [animal]. *)
val set_energy : animal -> int -> unit

(** [set_nutrition] sets the nutrition level of the given [animal]. *)
val set_nutrition : animal -> int -> unit

(** [set_money] sets the money level of the given [animal]. *)
val set_money : animal -> float -> unit

(** [set_microchip] adds a microchip to the given [animal]. *)
val set_microchip : animal -> unit

(** [set_leash] adds a leash to the given [animal]. *)
val set_leash : animal -> unit

(** [set_sickness] adds the given [sickness] to the given [animal]. *)
val add_sickness : animal -> string -> unit

(** [remove_sickness] removes the given [sickness] from 
    the given [animal] if it has it. *)
val remove_sickness : animal -> string -> unit

(** [get_microchip] returns whether or not the given [animal] has a microchip. *)
val get_microchip : animal -> bool

(** [get_leash] returns whether or not the given [animal] has a leash. *)
val get_leash : animal -> bool

(** [get_sickness] returns whether or not the given [animal]
     has the given [sickness]. *)
val get_sickness : animal -> string -> bool

(** [get_name] returns the name of the given [animal]. *)
val get_name : animal -> string

