type ('a, 'b) t = ('a, 'b) result

let return x = Ok x

let bind f = function
  | Ok x -> f x
  | e    -> e

let (>>=) x f = bind f x

let map f = function
  | Ok x -> Ok (f x)
  | e    -> e

let get_exn = function
  | Ok x -> x
  | Error _ -> failwith "Result.get_exn: Error"
