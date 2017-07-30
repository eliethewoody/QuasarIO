Random.init (int_of_float (Unix.time ()));;
let (|>) x f = f x;;
let failwith msg = raise (Failure msg);; 

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j [] ;;

let bin_of_dec x =
	let rec __bin_of_dec y accumulator = 
		match y with 
			| 0 -> accumulator
			| _ -> __bin_of_dec (y/2) ((y mod 2)::accumulator) in
__bin_of_dec x [];; 

let oct_of_dec x = 
  let rec __oct_of_dec y accumulator = 
    match y with 
      | 0 -> accumulator
      | _ -> __oct_of_dec (y/8) ((y mod 8)::accumulator)  in 
__oct_of_dec x [];;

(*let partial_invert ?starton:(starton=0) binary_acii delimiter = (*если что, то можно добавить это: ?fake_filled_character:(fake_filled_character=[]) *)*)
let partial_invert delimiter binary_acii =
  let rec __partial_invert code iteration accumulator = 
    match code with
      | [] -> accumulator
      | (digit::rest) -> if iteration mod delimiter == 0 then (*здесь происходит инверсия каждого n-ого бита, справа налево (№небагафича) *)
                           match digit with 
                             | 1 -> __partial_invert rest (1+iteration) (0::accumulator) 
                             | 0 -> __partial_invert rest (1+iteration) (1::accumulator)
                             | _ -> failwith "badarg"
                          else
                             __partial_invert rest (1+iteration) (digit::accumulator) in 
 (*__partial_invert binary_acii starton [];;*)
 __partial_invert binary_acii 0 [];;
 

let generate_whole_symbol character header = 
  let binary_acii = 0::(bin_of_dec (Char.code character)) in
  let rec __generate_whole_symbol accumulator header = (*по традиции, аккумулятор тоже формируется справа налево (№небагафича) *)
    match header with 
      | [] -> accumulator 
      | (delimiter::rest) -> __generate_whole_symbol ((partial_invert delimiter binary_acii)::accumulator) rest in 
(header, (__generate_whole_symbol [] header));;

let perform_writable_form (_, body) =  (*FIXME FIXME*)
  (*слегка костылим с типчиками ФИКСМЕ: найти способ типа List.join*)
  (*Эй, окоянный! коль ты человек живой, смотришь на сие и ужасаешься, то перепиши, а не глумись впустую!
    Hey! If you are a living person, you look at this and you are horrified, then rewrite it, and do not mock for nothing!*)
  List.map (fun x -> (String.concat "" (List.map string_of_int x))) (List.map oct_of_dec (List.map (fun x -> int_of_string (String.concat "" (List.map string_of_int x))) body)) 
  (*при расшифровке надо смотрети на длину бинарника. если < 8, то в начало пихануть 0*)

let encrypt_text text_by_letters headers = 
  List.map2 (fun symbol header -> perform_writable_form (generate_whole_symbol symbol header)) text_by_letters headers;;

(*let encrypt_assoc_text text_by_letters dictionary = *)

let generate_headers length_of_message = 
  let rec __make_header num acc =  
    match num with 
      | 0 -> acc
      | _ -> __make_header (num-1) ((List.map (fun _ -> (Random.int 7)+1) (0--7))::acc) in 
  if length_of_message > 0 then __make_header length_of_message [] else failwith "badarg";;
    
let make_dict = 
  let d = Hashtbl.create 26 in 
    List.iter2 (fun x y -> Hashtbl.add d x y) (List.map (fun z -> Char.chr z) (97--122)) (generate_headers 26);
    d;;

let fill_with_zeroes arr = 
  let rec __fill acc = 
    match List.length acc with 
      | 8 -> acc
      | _ -> __fill (0::acc) in 
  __fill arr;;

let decode_symbol symbol header = 
  let permutate sybmol_str header_digit = 
    print_endline ("0o"^sybmol_str);
    ("0o"^sybmol_str)
      |> int_of_string
      |> string_of_int
      |> explode
      |> List.map (fun z -> int_of_string (String.make 1 z))
      |> fill_with_zeroes
      |> partial_invert header_digit
      |> List.map (fun z -> string_of_int z) 
      |> String.concat "" in 
  List.map2 (fun x y -> Char.chr (int_of_string ("0b"^(permutate x y)))) symbol header 




(*доделывай и рефакторь, пидор!*)

(*
val bin_of_dec: int -> int list
val oct_of_dec: int -> int list
val partial_invert: ?starton:int -> int list -> int -> int list
val generate_whole_symbol: char -> int list -> (int list * int list list)
val perform_writable_form: ('a * int list list) -> int list list 
val encrypt_text: char list -> int list list -> int list list 
val generate_headers: int -> int list list
*)