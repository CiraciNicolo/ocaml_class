type 'a mylist = Nil | Cons of 'a * 'a mylist
(*example Cons(type, Cons(type, Nil))*)

let map f l =
    let rec mapr = function
    | Nil -> Nil
    | Cons(h, tl) -> Cons(f h, mapr tl)
in mapr l

let append l1 l2 =
    let rec apprendr l1 l2 =
    match l1,l2 with
    | Cons(h, t), _ -> Cons(h, apprendr t l2)
    | Nil, Cons(h, t) -> Cons(h, apprendr Nil t)
    | Nil, Nil -> Nil
in apprendr l1 l2

type small = Four | Three | Two | One

let lt_small a b =
match (a<b) with
| true -> false
| false -> true

type unop = Neg
type binop = Add | Sub | Mul | Div
type exp = Constant of int | Unary of unop * exp | Binary of exp * binop * exp

let eval exp =
    let rec evalr = function
    | Constant v -> v
    | Unary (unop, exp)-> - (evalr exp)
    | Binary (h, binop, t) -> match binop with
                                        | Add -> (evalr h) + (evalr t)
                                        | Sub -> (evalr h) - (evalr t)
                                        | Mul -> (evalr h) * (evalr t)
                                        | Div -> (evalr h) / (evalr t)
in evalr exp
