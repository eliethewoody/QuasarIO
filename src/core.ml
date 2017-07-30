open Rencralga;;
let elem = fun l -> String.concat " " (List.map string_of_int l)
let rec row = function
  | [] ->  []
  | x :: tl -> elem x :: row tl;;
let print n = fun l -> print_string (String.concat "\n" (row l));
                       let oc = open_out n in 
                        Printf.fprintf oc "%s\n" (String.concat "\n" (row l));
                        close_out oc;;
let msg = explode "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod 
                   tempor incididunt ut labore et dolore magna aliqua. Interdum consectetur 
                   libero id faucibus nisl tincidunt. Adipiscing enim eu turpis egestas pretium 
                   aenean pharetra. Non enim praesent elementum facilisis leo vel fringilla. 
                   Vulputate dignissim suspendisse in est. Pellentesque eu tincidunt tortor 
                   aliquam nulla facilisi cras fermentum. Quis vel eros donec ac odio tempor. 
                   Mauris nunc congue nisi vitae suscipit tellus mauris a. Amet consectetur adipiscing 
                   elit duis tristique. Sed viverra tellus in hac. Lacinia at quis risus sed vulputate 
                   odio ut enim blandit. Libero nunc consequat interdum varius. Arcu cursus euismod 
                   quis viverra nibh. Congue quisque egestas diam in arcu cursus. Cursus turpis massa
                   tincidunt dui. Nec tincidunt praesent semper feugiat nibh sed. Vitae congue eu 
                   consequat ac." in
let hdrs = generate_headers (List.length msg) in 
let c = encrypt_text msg hdrs in
print_endline "заголовки символов:";
print "head.txt" hdrs;
print_endline "";
print_endline "символы:";
print "data.txt" c;
print_endline "";
print_endline "расшифрованный текст:";
let v = decrypt_text c hdrs in 
List.iter (fun z -> print_string (String.make 1 z)) v;
print_endline "";


