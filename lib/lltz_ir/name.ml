let counter = ref 0

let create () =
  let name = "name_var_4sfa9wjas8" ^ string_of_int !counter in
  counter := !counter + 1;
  name
;;
