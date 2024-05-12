

(** Age of the plant: can be Empty, Young, Mid, or Old *)
type value = None | Small | Medium | Big

(** Plant type where each plant has a mutable age of type age *)
type t = {
  mutable value : value;
}

(** Creates a new plant instance with the age set to Empty *)
val empty_coin : unit -> t

val get_value : t -> value

(** Converts a plant to its string representation and pads with spaces *)
val coin_to_string : t -> string

(** Randomly updates the age of the plant, returns it to empty (if dead), or plant stays the same *)
val increasing : t -> unit

