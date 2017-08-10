open Encralgb;;
open Io;;
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
exp (String.length s - 1) [];;
let key = [1;1;1;0;1;1;0;0;] in 
let source = explode (read_full_file "./a.txt") in 
let headers = Alg.generate_headers (List.length source) in 
let safe = Api.encrypt_text source headers in 
record_message safe "./safe.q";
record_key headers "./key.q" key;
let r_headers = read_encrypted_key "./key.q" key; in
let r_safe = read_encrypted "./safe.q"; in 
print r_headers;
print_endline "\n--------------------------";
print r_safe;
print_endline "\n--------------------------";
List.iter print_char (Api.decrypt_text r_safe r_headers);