(****************************100 symbols ethalon**************************************************)
(*
*File    : rencragla.ml
*Project : QuasarIO v0.3.16
*Created : SUN JUL, 30, 2017
*DLM     : SUN JUL, 30, 2017
*Purpose : refactor and improve `awesome` code of encralga.ml
*Author  : Kaganovich Maksim <eliethewoody@ya.ru>
*Copyright by Kaganovich Maksim (c) 2017. Powered by the MIT public licence
*)
(*`Service` functions are represented below i.e. Custom operators and converting functions*)
type t = int list list;; 
Random.init (int_of_float (Unix.time ()));;
let failwith msg = raise (Failure msg);; 
let (|>) x f = f x;;
let (--) i j = let rec aux n accumulator = if n < i then accumulator else aux (n-1) (n :: accumulator) in aux j [];;
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;
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
__oct_of_dec x [];;
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
(*Algorithm is down below*)
let partial_invert binary_acii_code delimiter = 
  let __partial_invert current_bit number_of_current_bit = 
    if (number_of_current_bit mod delimiter) == 0 then 
    (*здесь происходит инверсия каждого n-ого бита, справа налево (№небагафича)*)
      match current_bit with 
        | 1 -> 0 
        | 0 -> 1
        | _ -> failwith "badarg"
      else current_bit in 
  (*Внимание: костыль! использование списка чисел для получение номера текущего бита ln44*)
List.map2 (fun x y -> __partial_invert x y) binary_acii_code (0--7);;




let encoded_form_of character header = (*TODO: при переводе это уйдёт. Само. *)
  let binary_acii = fill_with_zeroes (0::(bin_of_dec (Char.code character))) in
    (List.map (fun x -> partial_invert binary_acii x) header)
      |> List.map (fun a -> List.map (fun b -> string_of_int b) a)
      |> List.map (fun a -> String.concat "" a)
      |> List.map (fun a -> int_of_string a)
      |> List.map (fun a -> oct_of_dec a);;
let encrypt_text text_by_letters headers = 
  List.map2 (fun symbol header -> encoded_form_of symbol header) text_by_letters headers;;
let generate_headers length_of_message = 
  let rec __make_header num accumulator =  
    match num with 
      | 0 -> accumulator
      | _ -> __make_header (num-1) ((List.map (fun _ -> (Random.int 7)+1) (0--7))::accumulator) in 
      (*Внимание: костыль! использование списка чисел для генерации нужного количества заголовков ln55*)
if length_of_message > 0 then __make_header length_of_message [] else failwith "badarg";;
let decoded_form_of encoded_symbol header = 
  let replicated_chars = 
    (List.map2 (fun x y -> partial_invert x y) (dec_of_oct encoded_symbol) header)
      |> List.map (fun a -> string_of_int a)
      |> List.map (fun a -> Char.chr (int_of_string ("0b"^a))) in 
  List.hd replicated_chars;;
let decrypt_text text_by_letters headers = 
  List.map2 (fun symbol header -> decoded_form_of symbol header) text_by_letters headers
