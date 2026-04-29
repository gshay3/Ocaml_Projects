(* Converts a string into a list of characters *)
let explode (s : string) : char list =
  List.init (String.length s) (String.get s)