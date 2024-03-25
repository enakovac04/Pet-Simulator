(** [animal] represents the type of pet *)
type animal = Camel [@@warning "-32"]

type pet = {
  animal : string;
  name : string;
  mutable health : int;
  mutable money : float;
}
[@@warning "-32"]
(** [pet] is the pet of type [animal] with identifier [name], [health] status
    out of 10 where 0 means dead and 10 means perfect health, and amount of
    [money] in USD *)

val create : string -> string -> int -> float -> pet
[@@warning "-32"]
(** [create] makes a new pet *)

val update_health : string -> int -> pet
[@@warning "-32"]
(** [update_health name amount] changes the health of the pet with the given
    [name] by [amount]. If [amount] is negative, it represents decrementing
    health. If amount makes health less than zero or greater than 10, health is
    set to zero or 10, respectively. *)

val update_money : string -> int -> pet
[@@warning "-32"]
(** [update_money name amount] changes the total money for the pet with the
    given [name] by [amount]. If [amount] is negative, it represents losing
    money. Raises "Not enough money" error if amount pushes money into the
    negative*)

val to_string : pet -> string
[@@warning "-32"]
(** [to_string p] converts the pet [p] to the string format with fields of pet record. 
    For example: if user_pet = {animal = Camel; name = "Carly"; health = 6; money = 17.}, 
      then to_string user_pet yields "Camel Carly has 6/10 health and $17.00"*)

val exercise_pet : string -> int -> float -> pet
[@@warning "-32"]
(** [exercise_pet name health_gain cost] makes the pet with the given [name]
    exercise, increasing its health by [health_gain] and decreasing its money by
    [cost]. *)

val feed_pet : string -> int -> pet
[@@warning "-32"]
(** [feed_pet name amount] feeds the pet with the given [name], increasing its
    health by [amount] and decreasing its money by [cost] *)

val calculate_health : string -> pet
[@@warning "-32"]
(** [calculate_health name] calculates the health status of the pet with the
    given [name] based on a variety of factors such as food, water, exercise. *)

val revive_pet : string -> pet
[@@warning "-32"]
(** [revive_pet name] revives the dead pet with the given [name], setting its
    health to a default value. *)

val age_pet : string -> int -> pet
[@@warning "-32"]
(** [age_pet name years] ages the pet with the given [name] by [years],
    affecting its health or other attributes. *)

val print_set : pet -> unit
[@@warning "-32"]
(** [print_set p] prints the pet [p] using print_endline and the to_string
    function (see spec above)*)
