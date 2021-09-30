open Base
open Stdio

let explode s = String.to_list s

let parse_count str = 
  match String.split str ~on:'-' with 
  | first::second :: _ ->  (Int.of_string first, Int.of_string second)
  | _ -> (-1,-1)


let rec count str pattern = 
  match str with
    | first :: rest -> ( count rest pattern ) + (if (Char.equal pattern  first) then 1 else 0)
    | [] -> 0


let get_first str = 
  match explode str with 
  | first :: _ -> first
  | _ -> '2'

let valid str = 
  match String.split str ~on:' ' with 
  | first::second::third :: _ -> 
      let (l, h) = parse_count first in 
        let ar = String.to_array third in 
          let cr = get_first second in 
            if ((Char.equal ar.(l) cr) <> (Char.equal ar.(h) cr)) then 1 else 0
  | _ -> 0


let rec main count  =
  let line = In_channel.input_line In_channel.stdin in
  match line with 
  | None -> count
  | Some x -> main (count + valid x)

let () = 
  let value =  main 0 in 
      printf "Answer is %d \n" value

