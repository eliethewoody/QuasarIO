type t 
val ( -- )               : int       -> int        -> int list
val ( |> )               : 'a        -> ('a -> 'b) -> 'b
val explode              : string    -> char list

val encrypt_text         : char list -> t          -> t 

val decrypt_text         : t         -> t          -> char list