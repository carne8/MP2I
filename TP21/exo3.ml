let hash_int k m = k mod m
(* Q2: Aucune idée *)
let hash_string str m =
  if String.length str = 0 then 0 else
  let s = str.[0] |> int_of_char |> ref in
  for i = 1 to String.length str - 1 do
    s := (!s + int_of_char str.[i]) * 256
  done;
  !s mod m

let hash m x = Hashtbl.hash x mod m

let () =
  let m = 100 in
  Printf.printf "Gros: %i\n" (hash_string "Gros" m);
  Printf.printf "Gras: %i\n" (hash_string "Gras" m);
  Printf.printf "Gres: %i\n" (hash_string "Gres" m);
  Printf.printf "Gris: %i\n" (hash_string "Gris" m);

  Printf.printf "42: %i\n" (Hashtbl.hash 42);
  Printf.printf "43: %i\n" (Hashtbl.hash 43);
  Printf.printf "44: %i\n" (Hashtbl.hash 44);
  Printf.printf "45: %i\n" (Hashtbl.hash 45)

(* let length { size = m } = m *)