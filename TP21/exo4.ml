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
  d.size <- d.size * 2;
  d.hash <- hash d.size;
  let old_data = d.data in
  d.data <- Array.make d.size [];
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

let () =
  let d = create_hash_table 10 in
  add "toto" 0 d;
  add "toto" 1 d;
  add "toto" 2 d;
  add "toto" 3 d;
  add "toto" 4 d;
  add "toto" 5 d;
  add "toto" 6 d;
  add "toto" 7 d;
  add "toto" 8 d;
  resize d;
  assert (find "toto" d = 8); remove "toto" d;
  assert (find "toto" d = 7); remove "toto" d;
  assert (find "toto" d = 6); remove "toto" d;
  assert (find "toto" d = 5); remove "toto" d;
  assert (find "toto" d = 4); remove "toto" d;
  assert (find "toto" d = 3); remove "toto" d;
  assert (find "toto" d = 2); remove "toto" d;
  assert (find "toto" d = 1); remove "toto" d;
  assert (find "toto" d = 0); remove "toto" d