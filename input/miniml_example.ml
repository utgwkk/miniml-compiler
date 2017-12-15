let multiplicand = 8 in
let rec multiply8 = fun n ->
  if n < 1 then
    0
  else
    multiplicand + multiply8 (n + -1)
in
let apply = fun f -> fun x -> f x in
apply multiply8 9;;
