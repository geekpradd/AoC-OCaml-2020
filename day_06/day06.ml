open Base
open Stdio

let separator  = function
  | "" -> "\n"
  | _ -> ""
let rec accumulate str  =
  let line = In_channel.input_line In_channel.stdin in
  match line with 
  | None -> str
  | Some x -> accumulate (str ^ x ^ (separator x)  ) 

let rec count state = function 
  | [] -> 0
  | hd :: tl -> let enc = (Char.to_int hd) - (Char.to_int 'a') in 
                  if (state.(enc) = 1) then
                      count state tl
                  else begin
                      let () = state.(enc) <- 1 in
                        1 + count state tl
                  end 

let partial_count data = 
  let start = Array.create ~len:26 0 in 
    count start data
  
 let ans =   accumulate "" |> String.split ~on:'\n' |> List.map ~f: String.to_list |> List.map  ~f:(partial_count)  |> List.fold ~init:0 ~f:(+)


let f = printf "Answer is %d\n" ans 

