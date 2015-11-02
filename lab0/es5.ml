module String' =
struct
    let is_palindrome s =
        let l = String.length s in
        let rec verify n =
            n == 0 || (s.[l-n] == s.[n-1] && verify (n-1)) in
            verify (l/2)
    let ( - ) a b =
        let rec to_list s i =
            if (i == (String.length s)) then []
            else s.[i] :: (to_list s (i+1)) in
        let rec search s i l=
            if (i == (List.length l)) then ""
            else if (String.contains s (List.nth l i) == false) then (String.make 1 (List.nth l i)) ^ (search s (i+1) l)
            else "" ^ (search s (i+1) l)
        in search b 0 (to_list a 0)
    let anagram a dict =
        let count = 0 in
        List.iter (fun s -> if ((String.length ((a - s))) == 0) then Printf.printf "'%s\n" s) dict
end;;
