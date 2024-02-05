module type Monad = sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module ParserMonad : sig include Monad
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  val (let*): 'a t -> ('a -> 'b t) -> 'b t

  val (++): 'a t -> 'a t -> 'a t
  val (>>): 'b t -> 'a t -> 'a t
  val (<<): 'a t -> 'b t -> 'a t
  val (@>): 'a t -> 'a list t -> 'a list t
  val (<@): 'a list t -> 'a t -> 'a list t
  val (@@): 'a list t -> 'a list t -> 'a list t

  val run: 'a t -> char list -> 'a option

  val zero: 'a t
  val item: char t
  val sat: (char -> bool) -> char t
  val of_string: string -> char list
  val to_string: char list -> string
  val plus: 'a t -> 'a t -> 'a t
  val in_range: 'a -> 'a -> 'a -> bool
  val digit: char t
  val lower: char t
  val upper: char t
  val alpha: char t
  val alphanum: char t
  val char: char -> char t
  val char_set: string -> char t
  val left: 'a t -> 'b t -> 'a t
  val right: 'b t -> 'a t -> 'a t
  val word: char list -> char list t
  val str: string -> char list t
  val str_set: string list -> char list t
  val many1: 'a t -> 'a list t
  val many: 'a t -> 'a list t
  val option: 'a list t -> 'a list t
  val cons: 'a t -> 'a list t -> 'a list t
  val append: 'a list t -> 'a list t -> 'a list t
  val nat: int t
  val int: int t
  val sepby1: 'a t -> 'b t -> 'a list t
  val sepby: 'a t -> 'b t -> 'a list t
  val delim: 'a t -> 'b t -> 'c t -> 'b t
  val choice: 'a t list -> 'a t
  val consume: 'a t -> unit t
  val whitespace: char t
  val ignore: unit t
  val pre: 'a t -> 'a t
  val token: 'a t -> 'a t
  val eof: unit t
end 