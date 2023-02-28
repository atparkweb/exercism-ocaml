let rec sum list = match list with
  | [] -> 0
  | h::t -> h + sum(t);;

let rec unfold_right f init =
  match f init with
  | None -> []
  | Some (x, next) -> x :: unfold_right f next;;

let range n =
  let irange x = if x > n then None else Some (x, x + 1) in
    unfold_right irange 1;;

let square_of_sum n =
  let s = sum (range n) in
  s * s;;

let sum_of_squares n =
  let f x = x * x in
  List.map f (range n) |> sum;;

let difference_of_squares n =
  square_of_sum n - sum_of_squares n;;
