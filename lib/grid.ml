
type 'a t = Coin.t array array

let empty rows columns = 
  Array.init rows (fun _ -> Array.init columns (fun _ -> Coin.empty_coin()))

let grid_to_string grid generation =
  let rows = Array.length grid in
  let cols = if rows > 0 then Array.length grid.(0) else 0 in
  let row_border = "+" ^ String.make (4 * cols) '-' ^ "+\n" in
  let row_to_string row = "|" ^ (Array.to_list row 
                                |> List.map Coin.coin_to_string 
                                |> String.concat " " 
                                |> fun str -> str ^ " |") ^ "\n" in
  row_border ^ (Array.to_list grid |> List.map row_to_string |> String.concat "") ^ row_border 
              ^ string_of_int generation ^ "\n" 

let increasing grid =
  Array.iter (fun row -> Array.iter (fun coin -> Coin.increasing coin) row) grid

let count_big grid =
  Array.fold_left (fun acc row -> 
    acc + Array.fold_left (fun acc coin -> 
      if Coin.get_value coin = Big then acc + 1 else acc
    ) 0 row
  ) 0 grid
