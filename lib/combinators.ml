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
end = struct
  type 'a t = char list -> ('a * char list) option
  
  let return x = fun s -> Some (x, s)
  
  let bind m f = fun s ->
    match m s with
    | Some(x, s) -> (f x) s
    | None -> None
  let (>>=) = bind
  let (let*) = bind

  let run p str =
    match p str with
    | Some(parsed, _) -> Some(parsed)
    | None -> None
  
  let zero = fun _ -> None
  
  let item = function
  | x :: xs -> Some(x, xs)
  | [] -> None

  let sat pred = item >>= fun x -> if pred x then return x else zero
  
  let of_string s = String.to_seq s |> List.of_seq
  
  let to_string xs = List.to_seq xs |> String.of_seq

  let plus p q = fun s ->
    match p s with
    | Some(x, xs) -> Some(x, xs)
    | None -> q s

  let (++) = plus

  let in_range a b x = x >= a && x <= b
  let digit = sat (in_range '0' '9')
  let lower = sat (in_range 'a' 'z')
  let upper = sat (in_range 'A' 'Z')
  let alpha = lower ++ upper
  let alphanum = alpha ++ digit
  let char c = sat (fun x -> x = c)
  let char_set str =
    let xs = of_string str in
    sat (fun x -> List.mem x xs)

  let left p q = p >>= fun x -> q >>= fun _ -> return x
  let right p q = p >>= fun _ -> q >>= fun x -> return x

  let (>>) = right
  let (<<) = left

  let rec word xs = match xs with
  | [] -> return []
  | x :: xs ->
    let* x = char x in
    let* xs = word xs in
    return (x::xs)

  let str s = of_string s |> word
  let str_set xs =
    List.map str xs |> List.fold_left plus zero

  let rec many1 p =
    let* first = p in
    let* rest = many1 p ++ return [] in
    return (first::rest)

  let many p =
    many1 p ++ return []

  let option p =
    p ++ return []

  let cons xp xsp =
    let* x = xp in
    let* xs = xsp in
    return (x :: xs)

  (* right-associative *)
  let (@>) = cons
  (* left-associative *)
  let (<@) = fun xsp xp -> cons xp xsp

  let append xsp1 xsp2 =
    let* xs1 = xsp1 in
    let* xs2 = xsp2 in
    return (xs1 @ xs2)

  let (@@) = append

  let nat =
    let num c = (int_of_char c) - (int_of_char '0') in
    let eval str = List.fold_left (fun acc x -> 10 * acc + num x) 0 str in
    let* digits = many1 digit in
    return (eval digits)

  let int =
    let* f = (char '-' >> return (fun x -> -x)) ++ return (fun x -> x) in
    let* v = nat in return (f v)

  let sepby1 elem sep =
    let* v = elem in
    let* rest = many (sep >> elem) in
    return (v :: rest)

  let sepby elem sep =
    sepby1 elem sep ++ return []

  let delim d1 p d2 =
    d1 >> p << d2

  let choice xsp =
    List.fold_left plus zero xsp

  let consume p = p >> return ()
  let whitespace = char ' ' ++ char '\t' ++ char '\n' ++ char '\r'
  let ignore = consume (many whitespace)
  let pre p = ignore >> p
  let token p = p << ignore
  let eof = function
  | _ :: _ -> None
  | [] -> Some((), [])
end

