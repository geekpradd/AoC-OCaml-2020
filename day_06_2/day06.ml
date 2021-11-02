open Base
open Stdio

let separator  = function
  | "" -> "\n"
  | _ -> "*"
let rec accumulate str  =
  let line = In_channel.input_line In_channel.stdin in
  match line with 
  | None -> str
  | Some x -> accumulate (str ^ (separator x) ^ x ) 

let rec populate array = function 
  | [] -> array
  | hd :: tl -> let enc = (Char.to_int hd) - (Char.to_int 'a') in 
                  if (array.(enc) = 0) then
                    let () = array.(enc) <- 1 in 
                      populate array tl
                  else begin
                      populate array tl
                  end
let convert str = 
  let start = Array.create ~len:26 0 in 
     populate start (String.to_list str) |> Array.to_list
let rec combine list_1 = function 
  | [] -> []
  | hd :: tl -> 
    match list_1 with 
    | [] -> []
    | hd2 :: tl2 -> hd*hd2 :: (combine tl tl2)

let clear str = 
  let to_list = String.to_list str in 
    match to_list with
    | [] -> ""
    | hd :: tl -> if (Char.compare hd '*') = 0 then
                    String.of_char_list tl 
                    else begin
                        str 
                    end
      
let solve data = 
   List.map data ~f: convert |> List.fold ~f: combine ~init: (List.init 26 ~f:(fun _ -> 1)) |> List.fold ~f:(+) ~init:0 

 let ans = accumulate "" |> String.split ~on:'\n' |> List.map ~f: clear |> List.map ~f:(String.split ~on:'*') |> List.map ~f: solve  |> List.fold ~init:0 ~f:(+)


let f = printf "Answer is %d\n" ans


