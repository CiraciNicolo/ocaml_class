module Math' = struct
    let pi = 3.14159265358979312
    let rec fact = function
        | 0. -> 1.
        | n -> n *. fact (n-.1.)
    let rec power x = function
        | 0. -> 1.
        | n -> x *. power x (n-.1.)
    let sine x n =
        let rec siner x b n m =
            match m with
            | _ when (m > n) -> x
            | m ->
                if b == true then siner (x -. ((power x m) /. (fact m))) false n (m+.2.)
                else siner (x +. ((power x m) /. (fact m))) true n (m+.2.)
    in siner (x*.pi/.180.) true n 3.
end;;
