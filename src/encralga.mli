val bin_of_dec: int -> int list
val explode: string -> char list
val oct_of_dec: int -> int list
(*val partial_invert: ?starton:int -> int list -> int -> int list*)
val partial_invert: int -> int list -> int list
val generate_whole_symbol: char -> int list -> (int list * int list list)
val perform_writable_form: ('a * int list list) -> string list  
val encrypt_text: char list -> int list list -> string list list 
val generate_headers: int -> int list list
val make_dict: (char, int list) Hashtbl.t
val fill_with_zeroes: int list -> int list 
val decode_symbol: string list -> int list -> char list