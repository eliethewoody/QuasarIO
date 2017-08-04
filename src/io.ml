(****************************100 symbols ethalon**************************************************)
(*
*File    : io.ml
*Project : QuasarIO v0.3.16
*Created : SUN AUG, 1, 2017
*DLM     : SUN AUG, 1, 2017
*Purpose : the only place where QuasatIO interacts `the outer world`
*Author  : Kaganovich Maksim <eliethewoody@ya.ru>
*Copyright by Kaganovich Maksim (c) 2017. Powered by the MIT public licence
*)
open Rencralga;;
let join_ints x = int_of_string (String.concat "" (List.map string_of_int x));;
let split_ints x = List.map int_of_string (Str.split (Str.regexp "") (string_of_int x));;
let encode_xor_message message key = 
  let rec xor_digits line accumulator = 
    match line with 
      | [] -> accumulator
      | (digit::rest) -> xor_digits rest ((join_ints (List.map2 (fun a b -> a lxor b) 
                                                        (fill_with_zeroes (bin_of_dec digit)) (fill_with_zeroes key)))::accumulator) in 
  List.map (fun a -> xor_digits a []) message;;
let decode_xor_message message key = 
  let rec xor_digits line accumulator = 
    match line with 
      | [] -> accumulator
      | (digit::rest) -> xor_digits rest ((int_of_string ("0b"^(string_of_int (join_ints (List.map2 (fun a b -> a lxor b) 
                                            (fill_with_zeroes (split_ints digit)) (fill_with_zeroes key))))))::accumulator) in
  List.map (fun a -> xor_digits a []) message;;
let record_message message path = 
  let elem l = String.concat " " (List.map string_of_int l) in
  let rec row = function
    | [] ->  []
    | x :: tl -> elem x :: row tl in 
  let oc = open_out path in 
    Printf.fprintf oc "%s\n" (String.concat "\n" (row message));
  close_out oc;;
let record_key message path key = 
  let elem l = String.concat " " (List.map string_of_int l) in
  let rec row = function
    | [] ->  []
    | x :: tl -> elem x :: row tl in 
  let oc = open_out path in 
    Printf.fprintf oc "%s\n" (String.concat "\n" (row (encode_xor_message message key)));
  close_out oc;; 
(*cheers, https://rosettacode.org/wiki/Read_a_file_line_by_line#OCaml*)
let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None;;
let read_lines ic =
  let rec aux acc =
    match input_line_opt ic with
    | Some line -> aux (line::acc)
    | None -> (List.rev acc)
  in
  aux [];;
let lines_of_file filename =
  let ic = open_in filename in
  let lines = read_lines ic in
  close_in ic;
  (lines);;
let read_encrypted path = 
  let lines = lines_of_file path in                               
  List.map (fun a -> (List.map int_of_string (Str.split (Str.regexp " ") a))) lines;;
let read_encrypted_key path key =                       
  let full = List.map (fun a -> (List.map int_of_string (Str.split (Str.regexp " ") a))) (lines_of_file path) in 
  decode_xor_message full key;;
let read_full_file path =
  String.concat "" (lines_of_file path);;
let print message =
  let elem l = String.concat " " (List.map string_of_int l) in
  let rec row = function
    | [] ->  []
    | x :: tl -> elem x :: row tl in 
  Printf.printf "%s\n" (String.concat "\n" (row message))