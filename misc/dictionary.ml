module type DictionaryOf =
sig
    type t
end

module Dictionary (D: DictionaryOf) =
struct
    type value = None | Value of D.t
    let empty (k : string) = None
    let find d k = d k
    let add d k v = fun k' -> if k = k' then v else d k'
    let remove d k =
        if find d k = None then d
        else fun k' -> if k' = k then None else d k'
end

module Int' =
struct
        type t = int
end
