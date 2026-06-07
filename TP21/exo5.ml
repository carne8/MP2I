type ('a, 'b) hash_table = {
  mutable data : ('a * 'b) list array;
  mutable size : int;
  mutable hash : 'a -> int
}

let hash m k = Hashtbl.hash k mod m

let create_hash_table m =
  { data = Array.make m [];
    size = 0;
    hash = fun k -> Hashtbl.hash k mod m }

let mem k d = d.data.(d.hash k) |> List.mem_assoc k
let find k d = d.data.(d.hash k) |> List.assoc k

let resize d =
  print_endline "resize";
  let new_capacity = d.size * 2 in
  d.hash <- hash new_capacity;
  let old_data = d.data in
  d.data <- Array.make new_capacity [];
  old_data |> Array.iter (fun alveole ->
    alveole
    |> List.rev
    |> List.iter (fun (k, v) ->
      let hash = d.hash k in
      d.data.(hash) <- (k, v)::d.data.(hash)
    )
  )

let add k v d =
  let hash = d.hash k in
  d.data.(hash) <- (k, v)::d.data.(hash);
  d.size <- d.size + 1;
  if d.size / Array.length d.data > 2 then resize d

let remove k d =
  let rec remove_assoc key = function
    | [] -> false, []
    | (k, v)::tl when k = key -> true, tl
    | _::tl -> remove_assoc key tl
  in
  let hash = d.hash k in
  let removed, new_alveole = remove_assoc k d.data.(hash) in
  d.data.(hash) <- new_alveole;
  if removed then d.size <- d.size - 1

let replace k v { data; hash } =
  let rec assoc_replace k v = function
    | [] -> raise Not_found
    | (k', _)::tl when k' = k -> (k, v)::tl
    | _::tl -> assoc_replace k v tl
  in
  data.(hash k) <- assoc_replace k v data.(hash k)

(* O(n) avec n le nombre d'associations *)

let iter f { data } = Array.iter (List.iter (fun (k, v) -> f k v)) data


let string_to_mot str =
  str
  |> String.split_on_char ' '
  |> List.concat_map (String.split_on_char ',')
  |> List.concat_map (String.split_on_char '.')
  |> List.concat_map (String.split_on_char '\n')
  |> List.map String.lowercase_ascii

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let contains_str s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  if len_sub = 0 then true
  else if len_sub > len_s then false
  else
    let rec check_from i =
      if i + len_sub > len_s then false
      else if String.sub s i len_sub = sub then true
      else check_from (i + 1)
    in
    check_from 0

let () =
  let d = create_hash_table 100_000 in
  "Vingt_mille_lieues_sous_les_mers.txt"
  |> read_whole_file
  |> string_to_mot
  |> List.iter (fun mot ->
    if mot = "nos" then
      Printf.printf "%s: %i\n"
        mot
        (try find mot d with _ -> 0);

    if mem mot d then
      replace mot (find mot d + 1) d
    else
      add mot 1 d
  );

  d |> iter (fun mot count ->
    if count >= 100 then Printf.printf "%s: %i\n" mot count
  )