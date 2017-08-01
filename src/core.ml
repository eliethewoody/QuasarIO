open Rencralga;;
open Io;;
let source = explode (read_full_file "./a/lorem.txt") in 
let headers = generate_headers (List.length source) in 
let safe = encrypt_text  source headers in 
record_message safe "./a/safe.q";
record_message headers "./a/key.q";
let r_headers = read_encrypted "./a/key.q" in
let r_safe = read_encrypted "./a/safe.q" in 
print r_headers;
print_endline "\n-------------";
print r_safe;
print_endline "\n-------------";
List.iter print_char (decrypt_text r_safe r_headers);
