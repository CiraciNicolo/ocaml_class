let sum n m f =
    let rec sumr n m f s =
        match n with
        | _ when n > m -> s
        | _ -> sumr (n+1) m f (s + (f n))
  in sumr n m f 0;;

let rec (%%) n m=
    match m with
    | _ when m = 0 -> n
    | _ -> if n > m then gcd (n - m) m
             else gcd n (m - n)
