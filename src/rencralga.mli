val ( -- )               : int                        -> int                        -> int list
val ( |> )               : 'a                         -> ('a -> 'b)                 -> 'b
val bin_of_dec           : int                        -> int list   
val oct_of_dec           : int                        -> int   
val fill_with_zeroes     : int list                   -> int list
val explode              : string                     -> char list
val partial_invert       : int list                   -> int                        -> int list
val encoded_form_of      : char                       -> int list                   -> int list 
val encrypt_text         : char list                  -> int list list              -> int list list 
val generate_headers     : int                        -> int list list
val make_dict            : acii_number_start:int      -> acii_number_end:int        -> (char, int list) Hashtbl.t
val make_latin_dict      : (char, int list) Hashtbl.t
val encrypt_text_by_dict : char list                  -> (char, int list) Hashtbl.t -> int list list
val decoded_form_of      : int list                   -> int list                   -> char
val decrypt_text         : int list list              -> int list list              -> char list