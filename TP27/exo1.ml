let fib_descendant n =
  let t = Array.make (n+1) None in
  (* [fib k] renvoie le kième terme de la suite de Fibonacci
  si [t.(k)] contient un [Some], on renvoie directement sa valeur *)
  let rec fib k = (* int -> int *)
    match t.(k) with
    | Some v -> v (* on avait déjà calculé [fib k] *)
    | None ->
      t.(k) <- Some (if k < 2 then 1 else fib (k-1) + fib (k-2));
      Option.get t.(k)
  in fib n

(* Q3: O(n); Pile: O(n); Tas: O(n+1) *)

let fib_ascendant n =
  if n < 2 then 1 else
  let t = Array.make (n+1) 0 in
  t.(0) <- 1;
  t.(1) <- 1;
  for i = 2 to n do
    t.(i) <- t.(i-1) + t.(i-2)
  done;
  t.(n)

(* Q5: Temporelle: O(n); Pile: O(1); Tas: O(n+1) *)