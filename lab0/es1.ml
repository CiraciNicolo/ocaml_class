type element = {name: string; atomic_number: int};;
let alkaline_earth_metals = [
                                            {name= "barium"; atomic_number= 56};
                                            {name= "beryllium"; atomic_number= 4};
                                            {name= "calcium"; atomic_number= 20};
                                            {name= "magnesium"; atomic_number= 12};
                                            {name= "radium"; atomic_number= 88};
                                            {name= "strontium"; atomic_number= 38}
];;

let max l =
    let rec max atomic = function
        [] -> atomic
    |   h :: tl -> if (h.atomic_number > atomic.atomic_number) then max h tl else max atomic tl
    in max {name= "void"; atomic_number= 0} l;;

max alkaline_earth_metals;;

let rec atomic_sort = function
    [] -> []
|   h :: tl -> insert h (atomic_sort tl)
and insert head =  function
    [] -> [head]
|   h :: tl ->  if (head.atomic_number < h.atomic_number) then head :: h :: tl
                  else h :: insert head tl;;

atomic_sort alkaline_earth_metals;;

let noble_gasses  = [
                                {name= "argon"; atomic_number= 18};
                                {name= "helium"; atomic_number= 2};
                                {name= "krypton"; atomic_number=  36};
                                {name= "neon"; atomic_number= 10};
                                {name= "radon"; atomic_number= 86};
                                {name= "xenon"; atomic_number= 54};
];;

let rec name_sort = function
    [] -> []
|   h :: tl -> insert h (name_sort tl)
and insert head =  function
    [] -> [head]
|   h :: tl -> if (head.name < h.name) then head :: h :: tl
                 else h :: insert head tl;;

let rec merge_sort x y =
    match name_sort x, name_sort y with (* sort the list before merging *)
|    [], _ -> y
|   _, [] -> x
|   hx :: tlx, hy :: tly ->    if (hx.name < hy.name) then hx :: merge_sort tlx y
                                    else hy :: merge_sort x tly;;

merge_sort alkaline_earth_metals noble_gasses;;
