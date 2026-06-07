type 'a braun = V | N of 'a * 'a braun * 'a braun

(* --- Exercice 5 --- *)

let rec diff (a : 'a braun) (n : int) : int =
  match a with
  | V -> -n
  | N (_, g, d) ->
    match n mod 2 with
    | 0 -> diff d (n/2-1)
    | _ -> diff g (n/2)

let rec taille (a : 'a braun) : int =
  match a with
  | V -> 0
  | N(_, g, d) ->
    let taille_droit = taille d in
    1 + 2*taille_droit + diff g taille_droit

let rec hauteur (a : 'a braun) : int =
  match a with
  | V -> -1
  | N(_, g, _) -> 1 + hauteur g

let test_exercice_5 () =
  assert (diff V (-1) = 1);
  assert (diff V 0 = 0);
  assert (taille V = 0);
  assert (hauteur V = -1);

  let n1 = N(0, V, V) in
  assert (diff n1 0 = 1);
  assert (diff n1 1 = 0);
  assert (taille n1 = 1);
  assert (hauteur n1 = 0);

  let n2 = N(0, N(1, V, V), V) in
  assert (diff n2 1 = 1);
  assert (diff n2 2 = 0);
  assert (taille n2 = 2);
  assert (hauteur n2 = 1);

  let n3 = N(0, N(1, V, V), N(1, V, V)) in
  assert (diff n3 2 = 1);
  assert (diff n3 3 = 0);
  assert (taille n3 = 3);
  assert (hauteur n3 = 1)



(* --- Exercice 6 --- *)

let create : int braun = V

let is_empty = (=) V

let get_min (a : int braun) : int =
  match a with
  | V -> max_int
  | N(e, _, _) -> e

let rec insert (a : int braun) (x : int) : int braun =
  match a with
  | V -> N (x, V, V)
  (* | N(e, g, d) when x = e -> a *)
  | N(e, g, d) ->
    (* On insère à droite et on inverse les deux fils *)
    N(min e x, insert d (max e x), g)

let rec extract_element (a : int braun) : int * int braun =
  match a with
  | V -> failwith "Empty"
  | N(e, V, V) -> e, V
  | N(e, g, d) ->
    (* On enlève un élément à gauche et on inverse les deux fils
       pour conserver la propriété d'arbre de Braun *)
    let extracted, new_droit = extract_element g in
    extracted, N(e, d, new_droit)

let rec replace_min (a : int braun) (x : int) : int braun =
  match a with
  | V -> failwith "Pas possible"
  | N(e, V, V) -> N(x, V, V)
  | N(e, N(g, V, V), V) -> N(min x g, N(max x g, V, V), V)

  | N(_, N(_, _, _), V)
  | N(_, V, N(_, _, _)) -> failwith "Pas possible"

  | N(_, g, d) when x <= get_min g && x <= get_min d -> N(x, g, d)
  | N(_, g, d) when get_min g <= get_min d -> N(get_min g, replace_min g x, d)
  | N(_, g, d) (* get_min g > get_min d *) -> N(get_min d, g, replace_min d x)

let rec merge (g : int braun) (d : int braun) : int braun =
  match g, d with
  | g, V -> g
  | V, _ -> failwith "Pas possible"

  | N(eg, a1, a2), d when eg <= get_min d -> N(eg, d, merge a1 a2)
  | g, d ->
    let x, new_droit = extract_element g in
    let new_gauche = replace_min d x in
    N(get_min d, new_gauche, new_droit)

let extract_min (a : int braun) : int * int braun =
  match a with
  | V -> failwith "Empty"
  | N(e, g, d) -> e, merge g d


(** Valide la propriété d'arbre de Braun *)
let valide_braun = function
  | V -> true
  | N(_, g, d) ->
    let taille_g = taille g in
    let taille_d = taille d in
    taille_d <= taille_g && taille_g <= taille_d + 1

(** Vérifie la properiété de tas_min *)
let rec valide_tas_min = function
  | V -> true
  | N(e, g, d) ->
    match g with
    | V -> true
    | N(e', _, _) -> e <= e'
    &&
    match d with
    | V -> true
    | N(e', _, _) -> e <= e'
    && valide_tas_min g
    && valide_tas_min d

let test_exercice_6 () =
  (* get_min *)
  assert (get_min V = max_int);
  assert (get_min (N (0, V, V)) = 0);

  (* insert *)
  let a = N(1, N(2, V, V), V) in

  let b = insert a 4 in
  assert (valide_braun b);
  assert (valide_tas_min b);

  let c = insert (insert (insert b (-1)) 7) 4 in
  assert (valide_braun c);
  assert (valide_tas_min c);

  let arbres =
    List.init 1000 (fun i -> i)
    |> List.fold_left_map
      (fun acc i -> let acc' = insert acc i in acc', acc')
      create
    |> snd
  in
  arbres |> List.iter (fun arbre ->
    assert (valide_braun arbre);
    assert (valide_tas_min arbre)
  );

  (* extract_element *)
  assert (extract_element (N(0, V, V)) = (0, V));
  assert (extract_element (N(0, N(1, V, V), V)) = (1, N(0, V, V)));
  assert (extract_element (N(0, N(1, V, V), N(2, V, V))) = (1, N(0, N(2, V, V), V)));

  arbres |> List.iter (fun arbre ->
    let _, extracted_arbre = extract_element arbre in
    assert (valide_braun extracted_arbre);
    assert (valide_tas_min extracted_arbre);
  );

  (* replace_min *)
  assert (replace_min (N(0, V, V)) 5 = N(5, V, V));
  assert (replace_min (N(0, N(1, V, V), V)) 5 = (N(1, N(5, V, V), V)));
  assert (replace_min (N(0, N(1, V, V), N(2, V, V))) 5 = N(1, N(5, V, V), N(2, V, V)));

  arbres |> List.iter (fun arbre ->
    let replaced_arbre = replace_min arbre 42 in
    assert (valide_braun replaced_arbre);
    assert (valide_tas_min replaced_arbre)
  );

  (* megre *)
  assert (merge V V = V);
  assert (merge (N(0, V, V)) V = N(0, V, V));
  assert (merge (N(1, V, V)) (N(1, V, V)) = N(1, N(1, V, V), V));

  (* extract_min *)
  assert (
    let m, a' = extract_min a in
    m = get_min a
    && valide_braun a'
    && valide_tas_min a'
  );

  arbres |> List.iter (fun arbre ->
    let min, a = extract_min arbre in
    assert (min = get_min arbre);
    assert (valide_braun a);
    assert (valide_tas_min a)
  )


let () =
  test_exercice_5();
  test_exercice_6();
  print_endline "Tous les tests passent !"
