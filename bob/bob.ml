let is_question str =
  let str = String.trim str in
  let str_len = String.length str in
    match str_len with
      | 0 -> false
      | _ -> str.[str_len - 1] = '?'

let is_alpha c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

let remove_non_alpha str =
  let filtered_chars = String.fold_left (fun acc c ->
    if is_alpha c then acc ^ String.make 1 c else acc
  ) "" str in
  filtered_chars

let is_all_uppercase str =
  let str = remove_non_alpha str in
  let is_uppercase_char c = Char.uppercase_ascii c = c && is_alpha c in

  let rec check_chars idx =
    match idx with
    | idx when idx >= String.length str -> true
    | _ ->
      let current_char = str.[idx] in
      if is_uppercase_char current_char then
        check_chars (idx + 1)
      else
        false
  in

  if String.length str = 0 then
    false
  else
    check_chars 0

let is_empty str = (String.trim str |> String.length) = 0

let response_for str =
   let results = [is_empty str; is_question str; is_all_uppercase str;] in
   match results with
   | [true; false; false] -> "Fine. Be that way!"
   | [false; true; true]  -> "Calm down, I know what I'm doing!"
   | [false; true; false] -> "Sure."
   | [false; false; true] -> "Whoa, chill out!"
   | _                    -> "Whatever."
