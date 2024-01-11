include Parser_combinators.Combinators.ParserMonad
module PM = Parser_combinators.Combinators.ParserMonad

let dig = PM.run PM.int_list (PM.of_string "[45,-36,75,-222]") 

let _ = match dig with
| Some list -> List.iter (Printf.printf "%d,") list
| None -> Printf.printf "fail"