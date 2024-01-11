module type Monad = sig
    type 'a t
    val return: 'a -> 'a t
    val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module ParserMonad : sig include Monad
type ('a, 'b) parser

val (>>=): 'a t -> ('a -> 'b t) -> 'b t
val (++): 'a t -> 'a t -> 'a t
val (>>): 'a t -> 'a t -> 'a t
val (<<): 'a t -> 'a t -> 'a t
val run: ('a, 'b) parser -> 'a list -> 'b option

val zero: 'a t
val item: (char, char) parser
val sat: ('a -> bool) -> ('a, 'a) parser
val of_string: string -> char list
val plus: 'a t -> 'a t -> 'a t
val in_range: 'a -> 'a -> 'a -> bool
val digit: char t
val lower: char t
val upper: char t
val alpha: char t
val alphanum: char t
val char: char -> char t
val left: 'a t -> 'a t -> 'a t
val right: 'a t -> 'a t -> 'a t
val word: 'a list -> ('a, 'a list) parser
val str: string -> (char, char list) parser
val many1: 'a t -> 'a list t
val many: 'a t -> 'a list t
val sepby1: 'a t -> 'a t -> 'a list t
val sepby: 'a t -> 'a t -> 'a list t
val delim: 'a t -> 'a t -> 'a t -> 'a t
val nat: (char, int) parser
val int: (char, int) parser
val int_list: (char, int list) parser
val consume: 'a t -> unit t
val whitespace: char t
val ignore: unit t
val pre: 'a t -> 'a t
val token: 'a t -> 'a t
end 