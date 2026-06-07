type candidat = {
  mutable k: int;
  placement: int array;
      (* le candidat est représenté par les [k] premières cases de
         [placement] *)
}

let a_rejeter { placement = c; k } =
  let i = ref 0 and b = ref true in
  while !i < k - 1 && !b do
    b := c.(!i) <> c.(k-1) && abs (c.(!i) - c.(k-1)) <> k - 1 - !i;
    incr i
  done;
  not !b

let fixer_nouvelle_reine c j =
  c.placement.(c.k) <- j;
  c.k <- c.k + 1

let abandonner_derniere_reine c = c.k <- c.k - 1

let nombre_solutions_n_reines n =
  let rec parcours c =
    if a_rejeter c then 0
    else if c.k = n then 1
    else
      let res = ref 0 in
      for k = 0 to (n-1) do
        fixer_nouvelle_reine c k;
        res := !res + parcours c;
        abandonner_derniere_reine c
      done;
      !res
  in parcours { k = 0; placement = Array.make n 0 }

let solution_n_reines_opt n =
  let rec parcours c =
    if a_rejeter c then None
    else if c.k = n then Some c
    else
      let rec find_res k =
        if k >= n then None else begin
          fixer_nouvelle_reine c k;
          match parcours c with
          | Some c -> Some c
          | None ->
            abandonner_derniere_reine c;
            find_res (k+1)
        end
      in find_res 0
  in parcours { k = 0; placement = Array.make n 0 }

let solution_n_reines_opt n =
  let rec parcours c =
    if a_rejeter c then None
    else if c.k = n then Some c
    else
      List.init n (fun i -> i) |> List.find_map (fun k ->
        fixer_nouvelle_reine c k;
        match parcours c with
        | Some c -> Some c
        | None -> abandonner_derniere_reine c; None
      )
  in parcours { k = 0; placement = Array.make n 0 }
     |> Option.map (fun c -> c.placement)

let () =
  nombre_solutions_n_reines 0 |> print_int;
  print_newline()