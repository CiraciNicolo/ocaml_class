module type DictionaryOf =
sig
    type k
    type v
end

module Dictionary (D: DictionaryOf) =
struct
    type value = None | Value of D.v
    let empty (k : D.k) = None
    let find d k = d k
    let add d k v = fun k' -> if k = k' then (Value v) else d k'
    let remove d k =
        if find d k = None then d
        else fun k' -> if k' = k then None else d k'
end

module Int' =
struct
        type k = string
        type v = int
end
