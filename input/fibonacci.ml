loop v = (0, 1) in
  if 1000 < v.2 then v.2
  else recur (v.2, v.1 + v.2)
;;
