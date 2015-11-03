let append l1 l2 =
    let rec apprendr l1 l2 =
    match l1,l2 with
    | h::t, _ -> h :: (apprendr t l2)
    | [], h::t -> h :: (apprendr [] t)
    | [], [] -> []
in apprendr l1 l2
