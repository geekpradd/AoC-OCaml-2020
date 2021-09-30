open Base
open Stdio
let explode s = String.to_list s

let check line count = let c = (count %  Array.length line) in Bool.to_int (Char.equal line.(c) '#') 

let rec main count ans =
  let line = In_channel.input_line In_channel.stdin in
  match line with 
  | None -> ans
  | Some x -> main (count +  3) (ans + check (String.to_array x) count)

let () = 
  let value =  main 0 0 in 
      printf "Answer is %d \n" value

