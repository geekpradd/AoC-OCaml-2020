open Base
open Stdio

let separator  = function
  | "" -> "\n"
  | _ -> " "
let rec accumulate str  =
  let line = In_channel.input_line In_channel.stdin in
  match line with 
  | None -> str
  | Some x -> accumulate (str ^ x ^ (separator x)  ) 

let get_first entry = 
  match String.lsplit2 entry ~on:':' with
  | None -> ""
  | Some (x, _) -> x

let get_fields str = 
  let inp = String.strip str in 
  String.split inp ~on:' ' |> List.map ~f:get_first 

let check str entry = List.exists str ~f:(fun x -> String.equal x entry)
let check_valid input = let str = get_fields input in 
                          ((check str "byr") && (check str "iyr") && (check str "eyr") && (check str "hgt") && (check str "hcl") && (check str "ecl") && (check str "pid"))
 
let ans =  accumulate "" |> String.split ~on:'\n' |> List.map ~f: check_valid |> List.map ~f:Bool.to_int |> List.fold ~f:(+) ~init:0

 let () = printf "Answer is %d\n" ans
