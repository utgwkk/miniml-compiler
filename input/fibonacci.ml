loop v = (0, 1) in
  if 1000 < v.1 then v.1
  else recur (v.1, v.0 + v.1)
;;
