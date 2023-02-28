let is_triangle a b c =
  a * b * c > 0 &&
  a + b >= c &&
  b + c >= a &&
  a + c >= b;;

let is_equilateral a b c =
  if is_triangle a b c
  then
    a = b && b = c
  else false;;

let is_isosceles a b c =
  if is_triangle a b c
  then
    a = b || a = c || b = c
  else false;;

let is_scalene a b c =
  if is_triangle a b c
  then
    not (is_equilateral a b c) &&
    not (is_isosceles a b c)
  else false;;
