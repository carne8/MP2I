type graphe_l = int list array
type graphe_m = bool array array

let arc_est_dans_l (x, y) (g: graphe_l) =
  let rec list_contains v = function
    | [] -> false
    | x::tl -> x = v || list_contains v tl
  in
  list_contains y g.(x)

let arc_est_dans_m (x, y) (g: graphe_m) = g.(x).(y)

let successeurs_l x (g: graphe_l) = g.(x)
let successeurs_m x (g: graphe_m) =
  let rec loop i =
    if i >= Array.length g.(x) then
      []
    else begin
      if g.(x).(i) then
        i::loop (i+1)
      else
        loop (i+1)
    end
  in
  loop 0

let predecesseurs_l x (g: graphe_l) =
  let rec loop pred_candidate =
    if pred_candidate >= Array.length g then
      []
    else begin
      if List.mem x g.(pred_candidate) then
        pred_candidate::loop (pred_candidate+1)
      else
        loop (pred_candidate+1)
    end
  in
  loop 0
let predecesseurs_m x (g: graphe_m) =
  let rec loop pred_candidate =
    if pred_candidate >= Array.length g then
      []
    else begin
      if g.(pred_candidate).(x) then
        pred_candidate::loop (pred_candidate+1)
      else
        loop (pred_candidate+1)
    end
  in
  loop 0

let degre_sortant_l x (g: graphe_l) = List.length g.(x)
let degre_sortant_m x (g: graphe_m) =
  let rec loop acc i =
    if i >= Array.length g.(x) then
      acc
    else
      loop
        (if g.(x).(i) then acc+1 else acc)
        (i+1)
  in loop 0 0

let degre_entrant_l x (g: graphe_l) =
  let rec loop acc pred_candidate =
    if pred_candidate >= Array.length g then
      acc
    else
      loop
        (if List.mem x g.(pred_candidate) then acc+1 else acc)
        (pred_candidate+1)
  in
  loop 0 0
let degre_entrant_m x (g: graphe_m) =
  let rec loop acc pred_candidate =
    if pred_candidate >= Array.length g then
      acc
    else
      loop
        (if g.(pred_candidate).(x) then acc+1 else acc)
        (pred_candidate+1)
  in
  loop 0 0

let liste_vers_matrice (g: graphe_l) =
  let n = Array.length g in
  let m = Array.make_matrix n n false in

  g |> Array.iteri (fun i ->
    List.iter (fun j -> m.(i).(j) <- true)
  );

  m

let matrice_vers_liste (g: graphe_m) =
  let n = Array.length g in
  let l = Array.make n [] in

  g |> Array.iteri (fun i ->
    Array.iteri (fun j arc_existe ->
      if arc_existe then
        l.(i) <- j::l.(i)
    )
  );

  l

let parcours_profondeur (g: graphe_m) f x =
  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter x =
    if not vu.(x) then begin
      f x;
      vu.(x) <- true;
      g.(x) |> Array.iteri (fun succ arc_existe ->
        if arc_existe then visiter succ
      )
    end
  in
  visiter x

let parcours_profondeur_complet (g: graphe_m) f =
  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter x =
    if not vu.(x) then begin
      f x;
      vu.(x) <- true;
      g.(x) |> Array.iteri (fun succ arc_existe ->
        if arc_existe then visiter succ
      )
    end
  in
  g |> Array.iteri (fun x _ -> visiter x)


let () =
  let l = [| [1]; [0;1]; [0;1] |] in
  let m =
    [| [| false; true; false |];
       [| true; true; false |];
       [| true; true; false |] |] in

  assert (l |> liste_vers_matrice = m);
  assert (m |> matrice_vers_liste |> Array.map (List.sort compare) = l);

  assert (arc_est_dans_l (0, 0) l |> not);
  assert (arc_est_dans_l (0, 1) l);
  assert (arc_est_dans_l (0, 2) l |> not);
  assert (arc_est_dans_l (1, 0) l);
  assert (arc_est_dans_l (1, 1) l);
  assert (arc_est_dans_l (1, 2) l |> not);
  assert (arc_est_dans_l (2, 0) l);
  assert (arc_est_dans_l (2, 1) l);
  assert (arc_est_dans_l (2, 2) l |> not);
  assert (arc_est_dans_m (0, 0) m |> not);
  assert (arc_est_dans_m (0, 1) m);
  assert (arc_est_dans_m (0, 2) m |> not);
  assert (arc_est_dans_m (1, 0) m);
  assert (arc_est_dans_m (1, 1) m);
  assert (arc_est_dans_m (1, 2) m |> not);
  assert (arc_est_dans_m (2, 0) m);
  assert (arc_est_dans_m (2, 1) m);
  assert (arc_est_dans_m (2, 2) m |> not);

  assert (successeurs_l 0 l |> List.sort compare = [1]);
  assert (successeurs_l 1 l |> List.sort compare = [0; 1]);
  assert (successeurs_l 2 l |> List.sort compare = [0; 1]);
  assert (successeurs_m 0 m |> List.sort compare = [1]);
  assert (successeurs_m 1 m |> List.sort compare = [0; 1]);
  assert (successeurs_m 2 m |> List.sort compare = [0; 1]);

  assert (predecesseurs_l 0 l |> List.sort compare = [1;2]);
  assert (predecesseurs_l 1 l |> List.sort compare = [0; 1; 2]);
  assert (predecesseurs_l 2 l |> List.sort compare = []);
  assert (predecesseurs_m 0 m |> List.sort compare = [1;2]);
  assert (predecesseurs_m 1 m |> List.sort compare = [0; 1; 2]);
  assert (predecesseurs_m 2 m |> List.sort compare = []);

  assert (degre_entrant_l 0 l = 2);
  assert (degre_entrant_l 1 l = 3);
  assert (degre_entrant_l 2 l = 0);
  assert (degre_entrant_m 0 m = 2);
  assert (degre_entrant_m 1 m = 3);
  assert (degre_entrant_m 2 m = 0);

  assert (degre_sortant_l 0 l = 1);
  assert (degre_sortant_l 1 l = 2);
  assert (degre_sortant_l 2 l = 2);
  assert (degre_sortant_m 0 m = 1);
  assert (degre_sortant_m 1 m = 2);
  assert (degre_sortant_m 2 m = 2);

  let grand_graphe =
    [| [1;4];
       [2;3;4];
       [1;5];
       [1;4;6];
       [6];
       [7];
       [];
       [];
       [5;9];
       [6;8] |]
    |> liste_vers_matrice
  in
  parcours_profondeur grand_graphe (Printf.printf "%i\n") 0;
  parcours_profondeur_complet grand_graphe (Printf.printf "%i\n")