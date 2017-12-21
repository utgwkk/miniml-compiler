let rec f = fun x ->
  let rec g = fun y -> y
  in f g x
in f 1 2;;
