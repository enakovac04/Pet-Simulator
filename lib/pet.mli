type pet = {
  name : string;
  mutable health : int;
  mutable happiness : int;
  mutable energy : int;
  mutable nutrition : int;
  mutable money : float;
}
[@@warning "-32"]

(** [pet] is the pet of type [animal] with identifier [name], [health] status
    out of 10 where 0 means dead and 10 means perfect health, and amount of
    [money] in USD *)

type animal =
  | Camel of pet
  | Dog of pet
  
(** [animal] represents the type of pet *)

val create : string -> string -> animal
[@@warning "-32"]
(** [create] makes a new pet *)

val status_to_string : animal -> string
[@@warning "-32"]

val get_name : animal -> string

val options : string list
[@@warning "-32"]

val to_string : animal -> string

val health_to_string : animal -> string
(** [health_to_string] returns a string noting the amount of health the pet has *)

val happiness_to_string : animal -> string
(** [happiness_to_string] returns a string noting the amount of happiness the pet has *)

val energy_to_string : animal -> string
(** [energy_to_string] returns a string noting the amount of energy the pet has *)

val nutrition_to_string : animal -> string
(** [nutrition_to_string] returns a string noting the amount of nutrition the pet has *)

val money_to_string : animal -> string
(** [money_to_string] returns a string noting the amount of money the pet has *)

val decrease_health : animal -> int -> unit
(** [decrease_health] modifies the health of the animal by decreasing it by [amount]
    If the health falls below 0, it is set as 0 *)

val increase_health : animal -> int -> unit
(** [increase_health] modifies the health of the animal by increasing it by [amount]
    If the health falls above the max health, it is set as the max *)

val get_health : animal -> int
(** [get_health] returns the amount of health the pet has *)

val increase_happiness : animal -> int -> unit
val decrease_happiness : animal -> int -> unit
val increase_energy : animal -> int -> unit
val decrease_energy : animal -> int -> unit


val options : string list
(** [to_string p] converts the pet [p] to the string format with fields of pet record. 
    For example: if user_pet = Camel {name = "Carly"; health = 6; money = 17.}, 
      then to_string user_pet yields "Camel Carly has 6/10 health and $17.00"*)

(*val update_health : string -> int -> pet [@@warning "-32"]*)
(** [update_health name amount] changes the health of the pet with the given
    [name] by [amount]. If [amount] is negative, it represents decrementing
    health. If amount makes health less than zero or greater than 10, health is
    set to zero or 10, respectively. *)

(*val update_money : string -> int -> pet [@@warning "-32"]*)
(** [update_money name amount] changes the total money for the pet with the
    given [name] by [amount]. If [amount] is negative, it represents losing
    money. Raises "Not enough money" error if amount pushes money into the
    negative*)

(*val exercise_pet : string -> int -> float -> pet [@@warning "-32"]*)
(** [exercise_pet name health_gain cost] makes the pet with the given [name]
    exercise, increasing its health by [health_gain] and decreasing its money by
    [cost]. *)

(*val feed_pet : string -> int -> pet [@@warning "-32"]*)
(** [feed_pet name amount] feeds the pet with the given [name], increasing its
    health by [amount] and decreasing its money by [cost] *)

(*val calculate_health : string -> pet [@@warning "-32"]*)
(** [calculate_health name] calculates the health status of the pet with the
    given [name] based on a variety of factors such as food, water, exercise. *)

(*val revive_pet : string -> pet [@@warning "-32"]*)
(** [revive_pet name] revives the dead pet with the given [name], setting its
    health to a default value. *)

(*val age_pet : string -> int -> pet [@@warning "-32"]*)
(** [age_pet name years] ages the pet with the given [name] by [years],
    affecting its health or other attributes. *)

(*val print_set : pet -> unit [@@warning "-32"]*)
(** [print_set p] prints the pet [p] using print_endline and the to_string
    function (see spec above)*)
