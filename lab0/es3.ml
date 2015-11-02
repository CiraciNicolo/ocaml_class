module Matrix =
struct
    let zeroes n m = Array.make_matrix n m 0
    let identity n =  Array.init n (fun i -> Array.init n (fun j -> if i == j then 1 else 0))
    let init n = Array.init n (fun i -> Array.init n (fun j -> (i+1)*(j+1)))
    let transpose matrix = Array.init (Array.length matrix.(0)) (fun i -> Array.init (Array.length matrix) (fun j -> matrix.(j).(i)))
    let ( * ) a b = (*TIL: Matrix.( a * b ) with #use; or a * b with open*)
        let x = Array.length a in
        let y = Array.length b.(0) in
        let z = if y = 0 then 0 else Array.length b.(0) in
        let c = Array.make_matrix x z 0 in
        for i = 0 to x-1 do
          for j = 0 to z-1 do
            for k = 0 to y-1 do
              c.(i).(j) <- c.(i).(j) + a.(i).(k) * b.(k).(j)
            done
          done
        done;
        c
end;;
