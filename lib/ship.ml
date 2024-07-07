type ship_type = Number of int | Empty | One | Incorrect | Two
type t = { version : ship_type }

let to_string plant =
  match plant.version with
  | Empty -> " 🌵"
  | Number num -> " " ^ string_of_int num
  | One -> " 🐫"
  | Incorrect -> " ❌"
  | Two -> " 🐫"

let one () = { version = One }
let number n = { version = Number n }
let empty () = { version = Empty }
let incorrect () = { version = Incorrect }
let two () = { version = Two }
