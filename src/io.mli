val record_message : int list list -> string        -> unit
val input_line_opt  : in_channel    -> string option
val read_lines      : in_channel    -> string list
val lines_of_file   : string        -> string list
val read_encrypted  : string        -> int list list
val read_full_file  : string        -> string
val print           : int list list -> unit