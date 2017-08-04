open Rencralga;;
open Io;;
let source = explode (read_full_file "./a.txt") in 
let headers = generate_headers (List.length source) in 
let safe = encrypt_text source headers in 
record_message safe "./safe.q";
record_key headers "./key.q" [1;1;0;0;];
let r_headers = read_encrypted_key "./key.q" [1;1;0;0;]; in
let r_safe = read_encrypted "./safe.q"; in 
print r_headers;
print_endline "\n--------------------------";
print r_safe;
print_endline "\n--------------------------";
List.iter print_char (decrypt_text r_safe r_headers);