val ( -- ) : int -> int        -> int list
val ( |> ) : 'a  -> ('a -> 'b) -> 'b
module Alg: sig
  val join_ints        : int list -> int
  val split_ints       : int      -> int list
  val bin_of_dec       : int      -> int list
  val dec_of_bin       : int list -> int   
  val oct_of_dec       : int list -> int   
  val dec_of_oct       : int list -> int 
  val fill_with_zeroes : int list -> int list
  val partial_invert   : int list -> int           -> int list
  val encoded_form_of  : int      -> int list      -> int list 
  val generate_headers : int      -> int list list
  val decoded_form_of  : int list -> int list      -> int 
end
module Api: sig 
  val encrypt_text : char list     -> int list list -> int list list 
  val decrypt_text : int list list -> int list list -> char list
end


