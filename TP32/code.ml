(* Exercice 2 *)
(* Q1: 01257346 *)

let parcours_profondeur (g: int list array) x =
  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter x =
    if not vu.(x) then begin
      Printf.printf "%i\n" x;
      vu.(x) <- true;
      List.iter visiter g.(x)
    end
  in

  visiter x

let parcours_profondeur_matrice (g: bool array array) x =
  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter x =
    if not vu.(x) then begin
      Printf.printf "%i\n" x;
      vu.(x) <- true;
      Array.iteri
        (fun succ arc_existe -> if arc_existe then visiter succ)
        g.(x)
    end
  in

  visiter x


let parcours_profondeur pre post (g: int list array) x =
  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter x =
    if not vu.(x) then begin
      Printf.printf "%i\n" x;
      pre x;
      vu.(x) <- true;
      List.iter visiter g.(x);
      post x;
    end
  in

  visiter x

let ouvrir x = Printf.printf "Ouverture de %d\n" x (* prétraitement *)
let fermer x = Printf.printf "Fermeture de %d\n" x (* posttraitement *)

let accessible g x y =
  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter x =
    if x = y then
      true
    else if not vu.(x) then begin
      vu.(x) <- true;
      List.exists visiter g.(x)
    end else false
  in

  visiter x

let ex2 =
  [| [1;2];
     [2];
     [1] |]

let cyclique g =
  let n = Array.length g - 1 in
  let rec loop x =
    if x >= n then
      false
    else
      List.exists (fun succ -> accessible g succ x) g.(x)
      || loop (x+1)
  in
  loop 0

let connexe g =
  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter x =
    if not vu.(x) then begin
      vu.(x) <- true;
      List.iter visiter g.(x);
    end
  in

  visiter 0;
  Array.for_all Fun.id vu

(* Exercice 3 *)
let parcours_pseudo_profondeur g x =
  let stack = ref [x] in
  let push x = stack := x::!stack in
  let pop () =
    match !stack with
    | [] -> None
    | x::tl ->
      stack := tl;
      Some x
  in

  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter () =
    match pop () with
    | None -> ()
    | Some x when vu.(x) -> visiter ()
    | Some x ->
      Printf.printf "%i\n" x;
      vu.(x) <- true;
      List.iter push g.(x);
      visiter ()
  in

  visiter ()
  (* for x = 0 to n - 1 do visiter x done *)

let parcours_pseudo_profondeur_bis g x =
  let stack = Stack.create () in
  Stack.push x stack;

  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter () =
    match Stack.pop_opt stack with
    | None -> ()
    | Some x when vu.(x) -> visiter ()
    | Some x ->
      Printf.printf "%i\n" x;
      vu.(x) <- true;
      List.iter (fun succ -> Stack.push succ stack) g.(x);
      visiter ()
  in

  visiter ()

(* Parcours largeur *)
let parcours_largeur_t (g: int list array) (x: int) =
  let queue = Queue.create () in

  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter () =
    match Queue.take_opt queue with
    | None -> ()
    | Some x when vu.(x) -> visiter ()
    | Some x ->
      Printf.printf "%i\n" x;
      vu.(x) <- true;
      List.iter (fun succ -> Queue.add succ queue) g.(x);
      visiter ()
  in

  Queue.add x queue;
  visiter ()

(* Parcours largeur marquage précosse *)
let parcours_largeur_p (g: int list array) (x: int) =
  let queue = Queue.create () in

  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter () =
    match Queue.take_opt queue with
    | None -> ()
    | Some x ->
      Printf.printf "%i\n" x;
      List.iter (fun succ ->
        if not vu.(succ) then begin
          Queue.add succ queue;
          vu.(succ) <- true
        end
      ) g.(x);
      visiter ()
  in

  Queue.add x queue;
  vu.(x) <- true;
  visiter ()

(* Exercice 4 *)
type 'a tree = N of 'a * 'a tree list

let arbre_profondeur g x =
  let n = Array.length g in
  let vu = Array.make n false in

  let rec construit_arbre x =
    vu.(x) <- true;
    let children =
      g.(x) |> List.filter_map (fun succ ->
        if vu.(succ) then
          None
        else
          Some (construit_arbre succ)
      )
    in
    N(x, children)
  in

  construit_arbre x

let arbre_profondeur g x =
  let n = Array.length g in
  let vu = Array.make n false in

  let rec construit_arbre x =
    vu.(x) <- true;
    let children =
      g.(x) |> List.filter_map (fun succ ->
        if vu.(succ) then
          None
        else
          Some (construit_arbre succ)
      )
    in
    N(x, children)
  in

  construit_arbre x

let arbre_profondeur g x =
  let n = Array.length g in
  let vu = Array.make n false in

  let rec construire_arbre x =
    match vu.(x) with
    | true -> None
    | false ->
      vu.(x) <- true;
      let children =
        List.filter_map
          construire_arbre
          g.(x)
      in
      Some (N (x, children))
  in

  x
  |> construire_arbre
  |> Option.get

let tab_parents_largeur g x =
  let n = Array.length g in
  let parents = Array.make n (-1) in

  let queue = Queue.create () in
  let vu = Array.make n false in

  let rec visiter () =
    match Queue.take_opt queue with
    | None -> ()
    | Some (_, x) when vu.(x) -> visiter ()
    | Some (parent, x) ->
      parents.(x) <- parent;
      vu.(x) <- true;
      List.iter (fun succ -> Queue.add (x, succ) queue) g.(x);
      visiter ()
  in

  Queue.add (-1, x) queue;
  visiter ();
  parents

let chemin_opt g x y =
  let parents = tab_parents_largeur g x in

  let rec construit_liste acc z =
    if x = z then x::acc else
      construit_liste (z::acc) parents.(z)
  in

  match parents.(y) with
  | -1 -> None
  | p -> Some (construit_liste [y] p)

(* Exercice 5 *)
let parcours_profondeur (g: int list array) =
  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter x =
    if not vu.(x) then begin
      Printf.printf "%i\n" x;
      vu.(x) <- true;
      List.iter visiter g.(x)
    end
  in

  let rec relancer_parcours () =
    match Array.find_index (not) vu with
    | None -> ()
    | Some x ->
        print_newline ();
        visiter x;
        relancer_parcours ()
  in

  relancer_parcours ()

let parcours_largeur_complet (g: int list array) =
  let queue = Queue.create () in

  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter () =
    match Queue.take_opt queue with
    | None -> ()
    | Some x when vu.(x) -> visiter ()
    | Some x ->
      Printf.printf "%i\n" x;
      vu.(x) <- true;
      List.iter (fun succ -> Queue.add succ queue) g.(x);
      visiter ()
  in

  let rec relancer_parcours () =
    match Array.find_index (not) vu with
    | None -> ()
    | Some x ->
        print_newline ();
        Queue.add x queue;
        visiter ();
        relancer_parcours ()
  in

  relancer_parcours ()

let tableau_composantes g =
  let n = Array.length g in
  let vu = Array.make n false in
  let composantes = Array.init n Fun.id in

  let rec visiter cc x =
    if not vu.(x) then begin
      composantes.(x) <- cc;
      vu.(x) <- true;
      List.iter (visiter cc) g.(x)
    end
  in

  let rec relancer_parcours () =
    match Array.find_index (not) vu with
    | None -> ()
    | Some x ->
        visiter x x;
        relancer_parcours ()
  in

  relancer_parcours ();
  composantes

let composantes_connexes g =
  let n = Array.length g in
  let vu = Array.make n false in

  let rec visiter cc x =
    if not vu.(x) then begin
      cc := x::!cc;
      vu.(x) <- true;
      List.iter (visiter cc) g.(x)
    end
  in

  let rec relancer_parcours composantes =
    match Array.find_index (not) vu with
    | None -> composantes
    | Some x ->
        let cc = ref [] in
        visiter cc x;
        relancer_parcours (!cc::composantes)
  in

  relancer_parcours []