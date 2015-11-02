module type NumberADT =
sig
    type value
    val add: value -> value -> value
    val sub: value -> value -> value
    val per: value -> value -> value
    val div: value -> value -> value
end

module Number (N: NumberADT) =
struct
    let (+) x y = N.add x y
    let (-) x y = N.sub x y
    let ( * ) x y = N.per x y
    let ( / ) x y = N.div x y
end

module Complex =
struct
    type value = {re: float; im: float}
    let add x y = {re= x.re +. y.re; im= x.im +. y.im}
    let sub x y = {re= x.re -. y.re; im= x.im -. y.im}
    let per x y = {re= (x.re *. y.re) -. (x.im *. y.im); im= (x.re *. y.im) +. (x.im *. y.re)}
    let div x y = {re= ((x.re *. y.re) +. (x.im *. y.im)) /. ((y.re ** 2.) +. (y.im ** 2.)); im= ((x.im *. y.re) -. (x.re *. y.im)) /. ((y.re ** 2.) +. (y.im ** 2.))}
end
