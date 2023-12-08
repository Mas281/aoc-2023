open! Core

type 'a t

include Applicative.S with type 'a t := 'a t

module Open_on_rhs_intf : sig
  module type S
end

include
  Applicative.Let_syntax
  with type 'a t := 'a t
  with module Open_on_rhs_intf := Open_on_rhs_intf

val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
val ( <|> ) : 'a t -> 'a t -> 'a t

(* Combinators *)
val char : char -> char t
val string : string -> string t
val predicate : f:(char -> bool) -> string t
val consume : 'a t -> unit t
val consume_char : char -> unit t
val consume_line : unit t
val line : 'a t -> 'a t
val whitespace : unit t
val to_whitespace : string t
val to_eol : string t
val to_char : char -> string t
val int : int t
val any : 'a t -> 'a list t
val some : 'a t -> 'a list t
val list : 'a t -> sep:'b t -> 'a list t

(* Run *)
val run : 'a t -> string -> 'a Or_error.t
val run_exn : 'a t -> string -> 'a
