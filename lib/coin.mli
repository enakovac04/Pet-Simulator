

(** Value of the coin: can be None, Small, Medium, or Big *)
type value = None | Small | Medium | Big

(** Coin type where each coin has a mutable value of type value *)
type t = {
  mutable value : value;
}

(** Creates a new coin instance with the value set to None *)
val empty_coin : unit -> t

(** Gets the value of the coin *)
val get_value : t -> value

(** Converts a coin to its string representation and pads with spaces *)
val coin_to_string : t -> string

(** Randomly updates the value of the coin, returns it to none, or coin stays the same *)
val increasing : t -> unit

