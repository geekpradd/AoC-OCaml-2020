open Base
open Stdio

let rec find number list = 
  match list with 
  | [] -> None 
  | value :: rest -> (if value = number then Some(value) else find number rest)

let solve numbers = 
  match numbers with 
  | [] -> None 
  | value :: rest -> 
    (match find (2020 - value) rest with 
    | None -> None 
    | Some x -> Some(value*x))

let rec main numbers result =
  let line = In_channel.input_line In_channel.stdin in
  match line with 
  | None -> result
  | Some x ->
    match result with
    | None -> (match solve numbers with 
              | None -> main (Int.of_string x :: numbers ) result
              | Some y -> Some(y))
    | Some y -> Some(y)


let () = 
  match main [] None with 
  | None -> printf "Answer not found"
  | Some y -> printf "Answer is %d \n" y
