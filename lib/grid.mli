(** Grid type: 2D array of coins *)
type 'a t = Coin.t array array

(** Creates an empty grid with the specified number of rows and columns of a new empty_coin *)
val empty : int -> int -> Coin.t array array

(** Converts the grid and the generation number into a string representation for display *)
val grid_to_string : Coin.t array array -> int -> string

(** Applies the random increasing process to each coin in the grid *)
val increasing : Coin.t array array -> unit

(** Counts how many coins in the grid are of type Big *)
val count_big : Coin.t array array -> int
