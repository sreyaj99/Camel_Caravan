type ship_type
(** Defines a record with the possible types a camel can be or 
if the location being observed contains a number, an empty location, or
  an incorrect guess by the user. *)

type t
(** Defines the attributes of the camel which is the version of the camel *)

val to_string : t -> string
(** [to_string] is the string of the emoji representation of the 
    camel's location in the board at that moment *)

val one : unit -> t
(** [one] return a Ship of ship_type One *)

val number : int -> t
(** [number] return a Ship of ship_type Number *)

val empty : unit -> t
(** [empty] return a Ship of ship_type Empty *)

val incorrect : unit -> t
(** [incorrect] return a Ship of ship_type Incorrect *)

val two : unit -> t
(** [two] return a Ship of ship_type Two *)
