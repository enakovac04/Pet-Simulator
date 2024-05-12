Random.self_init()

type value = None | Small | Medium | Big

type t = {
  mutable value : value;
}

let empty_coin () =
  { value = None }

let get_value coin = coin.value

let value_to_string value = 
  match value with
  | None -> " "
  | Small -> "*"
  | Medium -> "+"
  | Big -> "$"

let coin_to_string coin = 
  " " ^ (value_to_string coin.value) ^ " "

let increasing coin =
  let random_num = 1 + Random.int 30 in
  match coin.value with 
  | None -> if random_num > 29 then coin.value <- Small
  | Small -> if random_num > 27 then coin.value <- Medium else if random_num < 5 then coin.value <- None
  | Medium -> if random_num > 27 then coin.value <- Big else if random_num < 7 then coin.value <- None
  | Big -> if random_num < 9 then coin.value <- None


