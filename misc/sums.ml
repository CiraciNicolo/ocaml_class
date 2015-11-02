module type Sum =
sig
    type t
    val sum: t -> t -> t
end

module Operation (T: Sum) =
struct
    let (+) x y = T.sum x y
end

module Int' =
struct
    type t = int
    let sum x y = x + y
end

module Float' =
struct
    type t = float
    let sum x y = x +. y
end

module String' =
struct
    type t = string
    let sum x y = x ^ y
end
