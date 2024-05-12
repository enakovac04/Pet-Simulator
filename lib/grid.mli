
(** Garden type: 2D array of plants *)
type 'a t = Coin.t array array

(** Creates an empty garden with the specified number of rows and columns of a new empty_plant *)
val empty : int -> int -> Coin.t array array

(** Converts the garden and its generation into a string representation for display *)
val grid_to_string : Coin.t array array -> int -> string

(** Applies the aging process to each plant in the garden *)
val increasing : Coin.t array array -> unit

val count_big : Coin.t array array -> int
