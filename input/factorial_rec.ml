let rec fac = fun n -> if n < 1 then 1 else n * fac (n + -1) in fac 10;;
