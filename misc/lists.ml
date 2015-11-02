let rec merge_i l1 l2 =
    match (l1,l2) with
    | [], _ -> l2
    | _,[] -> l1
    | h1::tl1, h2::tl2 -> if h1 < h2 then h1::(merge_i tl1 l2)
                                else h2::(merge_i l1 tl2);;

let rec merge f l1 l2 =
    match (l1,l2) with
    | [], _-> l2
    | _, [] -> l1
    | h1::tl1, h2::tl2 -> if f h1 h2 then h1::(merge f tl1 l2)
                                else h2::(merge f l1 tl2);;

type 'a mlist = {l: 'a list; f: ('a -> 'a -> bool); o: bool};;

let insert e ls =
    let rec insertr e l= match l with
    | [] -> [e]
    | h::tl -> if ls.f e h then e::l
                 else h::(insertr e tl)
in if ls.o then {ls with l=insertr e ls.l}
    else {ls with l=e::ls.l};;
