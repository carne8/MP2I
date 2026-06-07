(* Q1: 1 + 2 + 3 + ... + n | somme de 1 à n *)
(* . . . . *)

let somme t deb fin =
  let s = ref 0 in
  for i = deb to fin do
    s := !s + t.(i)
  done;
  !s

let max_somme_contigue_exhaustive t =
  let s = ref t.(0) in
  for l = 1 to Array.length t do
    for i = 0 to Array.length t - l do
      s := max !s (somme t i (i+l-1))
    done
  done;
  !s

(* Q4: O(n^3) *)

(* Q5: Chépa *)
(* Q6: f(n-1) = t[n-1]; f(i) = max(t[i], t[i] + f(i+1)) *)

(* Q7 *)
let rec f t i =
  if Array.length t - 1 = i then t.(i)
  else max t.(i) (t.(i) + f t (i+1))

let max_array t = t |> Array.fold_left max t.(0)

let max_somme_contigue_naive t =
  Array.init (Array.length t - 1) (f t)
  |> max_array

(* Q12: f t i est en O(n) et est appelé n fois
        max_array est en O(n)
        Donc le tout est en O(n^2) *)

(* --- Mémoïsation --- *)
let max_somme_contigue_sans_se_voiler_la_face t =
  let n = Array.length t in

  let result = ref t.(n-1) in
  let right = ref t.(n-1) in
  for i = n-2 downto 0 do
    right := max (t.(i)) (t.(i) + !right);
    result := max !result !right
  done;

  !result

let rec f_memo t cache i =
  if Hashtbl.mem cache i then
    Hashtbl.find cache i
  else begin
    let valeur_fi =
      if i = Array.length t - 1 then
        t.(i)
      else
        max t.(i) (t.(i) + f_memo t cache (i+1))
    in
    Hashtbl.add cache i valeur_fi;
    valeur_fi
  end

let max_somme_contigue_memoisee t =
  let n = Array.length t in
  let cache = Hashtbl.create n in
  Array.init (n - 1) (f_memo t cache) |> max_array

let f_memo2 t =
  let n = Array.length t in
  let cache = Hashtbl.create (n+1) in
  let rec f_memo i =
    match Hashtbl.find_opt cache i with
    | Some v -> v
    | None ->
      let valeur_fi =
        if i = Array.length t - 1 then
          t.(i)
        else
          max t.(i) (t.(i) + f_memo (i+1))
      in
      Hashtbl.add cache i valeur_fi;
      valeur_fi
  in
  f_memo

let max_somme_contigue_memoisee2 t =
  let n = Array.length t in
  Array.init (n - 1) (f_memo2 t) |> max_array

let () =
  (* let t = [| 13;-3;-25;20;-3;-16;-23;18;20;-7;12;-5;-22;15;-4;7 |] in *)

  let measure_time f x =
    let start_time = Sys.time () in
    let result = f x in
    let end_time = Sys.time () in
    let elapsed = end_time -. start_time in
    result, elapsed
  in

  let t = Array.init 1000 (fun _ -> Random.bits ()) in

  let r0, t0 = measure_time max_somme_contigue_exhaustive t in
  let r1, t1 = measure_time max_somme_contigue_naive t in
  let r2, t2 = measure_time max_somme_contigue_sans_se_voiler_la_face t in
  let r3, t3 = measure_time max_somme_contigue_memoisee t in
  let r4, t4 = measure_time max_somme_contigue_memoisee2 t in

  Printf.printf "exhaustive: %f ms\n" (t0 *. 1000.);
  Printf.printf "naive: %f ms\n" (t1 *. 1000.);
  Printf.printf "memoisee: %f ms\n" (t3 *. 1000.);
  Printf.printf "memoisee2: %f ms\n" (t4 *. 1000.);
  Printf.printf "sans_se_voiler_la_face: %f ms\n" (t2 *. 1000.);

  Printf.printf "\n%f; %f; %f; %f\n" (t0/.t2) (t1/.t2) (t3/.t2) (t4/.t2);

  assert(r0 = r1);
  assert(r0 = r2);
  assert(r0 = r3);
  assert(r0 = r4)