open Base
open Stdio
let explode s = String.to_list s

let check line count = let c = (count %  Array.length line) in Bool.to_int (Char.equal line.(c) '#') 

let rec main count ans1 ans2 ans3 ans4 ans5 parity =
  let line = In_channel.input_line In_channel.stdin in
  match line with 
  | None -> ans1*ans2*ans3*ans4*ans5
  | Some x -> let data = String.to_array x in 
              main (count +  1) (ans1 + check data count) (ans2 + check data (3*count))
              (ans3 + check data (5*count)) (ans4 + check data (7*count)) (if parity = 1 then ans5 else (ans5 + check data (count/2)))(1 - parity)

let () = 
  let value =  main 0 0 0 0 0 0 0 in 
      printf "Answer is %d \n" value

