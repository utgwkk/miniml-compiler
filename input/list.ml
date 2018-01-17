let nil = fun c -> fun n -> n in
let cons = fun x -> fun xs ->
  fun c -> fun n -> c x (xs c n) in
let foldr = fun n -> fun c -> fun l ->
  l c n in
let hd = fun l ->
  foldr (-42) (fun x -> fun y -> x) l in
let tuplel = fun x -> fun y -> fun z -> if z then x else y in
let fstl = fun t -> t true in
let sndl = fun t -> t false in
let tl = fun l ->
  sndl (foldr (tuplel nil nil) (fun x -> fun y ->
    (tuplel (cons x (fstl y)) (fstl y))) l) in
hd (tl (cons 1 (cons 2 (cons 3 nil))));;
