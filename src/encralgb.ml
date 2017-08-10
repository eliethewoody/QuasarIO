(****************************100 symbols ethalon**************************************************)
(*
 *File    : encralgb.ml
 *Project : QuasarIO v0.4.0
 *Created : WED AUG, 09, 2017
 *DLM     : WED AUG, 09, 2017
 *Purpose : enhance the encryption alg. add type support. prepare for translation
 *Author  : Kaganovich Maksim <eliethewoody@ya.ru>
 *Copyright by Kaganovich Maksim (c) 2017. Powered by the MIT public licence
 *)
(*TODO: add a bunch of exceptions and then raise them seems it is a pretty useful solution*)
Random.init (int_of_float (Unix.time ()));;
let failwith msg = raise (Failure msg);; 
let (|>) x f = f x;;
let (--) i j = let rec aux n accumulator = if n < i then accumulator else aux (n-1) (n :: accumulator) in aux j [];;
module Alg = struct 
  let join_ints x = int_of_string (String.concat "" (List.map string_of_int x));;
  let split_ints x = List.map int_of_string (Str.split (Str.regexp "") (string_of_int x));;
  let bin_of_dec x =
    let rec __bin_of_dec y accumulator = 
      match y with 
        | 0 -> accumulator
        | _ -> __bin_of_dec (y/2) ((y mod 2)::accumulator) in
  __bin_of_dec x [];; 
  let dec_of_bin x =
    let rec __dec_of_bin y accumulator = 
      match y with 
        | [] -> accumulator
        | (d::ds) -> __dec_of_bin ds (accumulator*2+d) in
  __dec_of_bin x 0;;
  let oct_of_dec x = 
    let rec __oct_of_dec y accumulator = 
      match y with 
        | 0 -> accumulator
        | _ -> __oct_of_dec (y/8) ((y mod 8)::accumulator)  in 
  int_of_string (String.concat "" (List.map (fun x -> string_of_int x) (__oct_of_dec (join_ints x) [])));;
  let dec_of_oct x = 
    let rec __dec_of_oct y accumulator = 
      match y with 
        | [] -> accumulator
        | (d::ds) -> __dec_of_oct ds (accumulator*8+d) in
  __dec_of_oct x 0;;
  let fill_with_zeroes arr = 
    let rec __fill accumulator = 
      match List.length accumulator with 
        | 8 -> accumulator
        | _ -> __fill (0::accumulator) in 
  __fill arr;;
  let generate_headers length_of_message = 
    let rec __make_header num accumulator =  
      match num with 
        | 0 -> accumulator
        | _ -> __make_header (num-1) ((List.map (fun _ -> (Random.int 7)+1) (0--7))::accumulator) in 
  if length_of_message > 0 then __make_header length_of_message [] else failwith "badarg";;
  let partial_invert binary_acii_code delimiter = 
    let __partial_invert current_bit number_of_current_bit = 
      if (number_of_current_bit mod delimiter) == 0 then 
        match current_bit with 
          | 1 -> 0 
          | 0 -> 1
          | _ -> failwith "badarg"
        else current_bit in 
  List.map2 (fun x y -> __partial_invert x y) binary_acii_code (0--7);;
  let encoded_form_of acii_code header = 
    let binary_acii = (fill_with_zeroes (bin_of_dec acii_code)) in 
    (List.map (fun x -> partial_invert binary_acii x) header)
      |> List.map (fun x -> oct_of_dec x);;
  let decoded_form_of code header = 
    let inverted_acii = List.map (fun x -> split_ints (dec_of_oct (split_ints x))) code in 
    List.hd
    ((List.map2 (fun x y -> partial_invert x y) inverted_acii header)
      |> List.map (fun x -> dec_of_bin x))    
end

module Api = struct
  let encrypt_text text_by_letters headers = 
    List.map2 (fun symbol header -> Alg.encoded_form_of (Char.code symbol) header) text_by_letters headers;;
  let decrypt_text text_by_letters headers = 
    List.map Char.chr (List.map2 (fun symbol header -> Alg.decoded_form_of symbol header) text_by_letters headers);;
end