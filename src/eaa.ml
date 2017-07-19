let digit_array_of s = Str.split (Str.regexp "") s

let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l (*stolen from stackoverflow*)

let bin_of_int d =
  if d < 0 then invalid_arg "bin_of_int" else
  if d = 0 then "0" else
  let rec aux acc d =
    if d = 0 then acc else
    aux (string_of_int (d land 1) :: acc) (d lsr 1) in
  String.concat "0" (aux [] d)

let generate_character_code character =
  let binary_acii = digit_array_of (bin_of_int (Char.code character)) in
    match binary_acii with
      | [] -> invalid_arg  "generate_character_code"
      | _ -> let rec indexer acc =
               match acc with
                 | [_; _; _; _; _; _; _; _] -> acc::[] (*FIXME: читай книгу - узнаешь, как поменять*)
                 | _ -> indexer ((string_of_int (Random.int 7))::acc) in
              let rec body (pos::hdr) binary_acii acc =
                let ipos = int_of_string pos in
                  match acc with
                    | [_; _; _; _; _; _; _; _] -> acc
                    | _ -> if (List.nth binary_acii ipos) = "0" then
                             body hdr binary_acii (replace binary_acii ipos "1") else
                             body hdr binary_acii (replace binary_acii ipos "0") in
              let code = indexer [] in
                body (List.nth code 0) binary_acii code









(*
[[1;4;3;0;1];[..];[..];[..];[..]]
*)
