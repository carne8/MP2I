type 'a abr =
  | V
  | N of 'a abr * 'a * 'a abr

let exemple_3 =
  N (
    N (
      N (
        N (V, -6., V),
        -5.1,
        N (V, -4.8, V)
      ),
      1.2,
      N (V, 3.1, V)
    ),
    3.5,
    N (
      N (V, 3.8, V),
      5.1,
      N (V, 10.5, V)
    )
  )

let rec max_abr = function
  | V -> failwith "No max"
  | N(_, e, V) -> e
  | N(_, e, d) -> d |> max_abr

let rec min_abr = function
  | V -> failwith "No min"
  | N(V, e, _) -> e
  | N(g, e, d) -> g |> min_abr

let rec est_abr = function (* Cette fonction parcours plusierus fois l'arbre entièrement *)
  | V -> true
  | N(V, e, V) -> true
  | N(g, e, V) -> max_abr g < e && est_abr g
  | N(V, e, d) -> e < min_abr d && est_abr d
  | N(g, e, d) ->
    max_abr g < e && e < min_abr d
    && est_abr g
    && est_abr d

let rec recherche x = function
  | V -> false
  | N(g, e, d) when x > e -> recherche x d
  | N(g, e, d) when x < e -> recherche x g
  | N(g, e, d) (* x = e *) -> true

let rec insere x abr =
  match abr with
  | V -> N(V, x, V)
  | N(g, e, d) when e < x -> N(g, e, insere x d)
  | N(g, e, d) when x < e -> N(insere x g, e, d)
  | N(g, e, d) -> abr

let rec abr_of_list = List.fold_left (fun abr e -> insere e abr) V

let rec infixe a =
  let rec infixe_aux a acc =
    match a with
    | V -> acc
    | N(g, e, d) ->
      acc
      |> infixe_aux d
      |> List.cons e
      |> infixe_aux g
  in
  infixe_aux a []

let (>>) f1 f2 = fun a -> a |> f1 |> f2
let tri = abr_of_list >> infixe

let () =
  assert (min_abr exemple_3 = -6.);
  assert (max_abr exemple_3 = 10.5);
  assert (est_abr exemple_3);
  assert (recherche 3. exemple_3 |> not);
  assert (recherche (-4.8) exemple_3);
  assert (insere (-4.7) exemple_3 |> recherche (-4.7));
  assert (insere (-4.8) exemple_3 = exemple_3);

  let list = [1;2;3;4;5] in
  let abr = list |> abr_of_list in
  assert (list |> List.for_all (fun e -> recherche e abr));

  assert ([5;3;4;2;1] |> tri = list)
