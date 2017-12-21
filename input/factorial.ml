loop v = (1, 10) in
  if v.2 < 1 then v.1
  else recur (v.1 * v.2, v.2 + -1)
;;
