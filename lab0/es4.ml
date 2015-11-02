#load "str.cma";;

let split = Str.split (Str.regexp_string " ")
let pack list =
    let rec aux current acc = function
      | [] -> []    (* Can only be reached if original list is empty *)
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (a :: current) acc t
         else aux [] ((a :: current) :: acc) t  in
    List.rev (aux [] [] list);;
let readFile filename =
    let in_channel = open_in filename in
    let words = ref [] in
    try
        while true do
            let line = input_line in_channel in
            words := List.rev_append (split line) !words
        done; []
    with End_of_file ->
        close_in in_channel;
        List.sort compare (List.map (fun s -> String.lowercase s) !words)
let interpretate list =
    let rec aux acc = function
        | [] -> acc
        | h::t -> aux ((List.length h , List.hd h) :: acc) t
    in aux [] (List.rev list)
