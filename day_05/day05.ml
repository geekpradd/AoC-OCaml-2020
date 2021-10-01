open Base
open Stdio

let rec parse ~on ~value = function 
  | [] -> value
  | hd :: tl -> parse ~on tl ~value: (2*value + if Char.equal hd on then 1 else 0 ) 
let get_value x = 
  let array = String.to_array x in 
    8*(Array.sub array ~pos:0 ~len:7 |> Array.to_list |> parse ~on:'B' ~value:0)+ (Array.sub array ~pos:7 ~len:3 |> Array.to_list |> parse ~on:'R' ~value:0)
let rec solve ans  =
  let line = In_channel.input_line In_channel.stdin in
  match line with 
  | None -> ans
  | Some x -> max ans (get_value x) |> solve


 let () = solve 0 |> printf "Answer is %d\n" 
