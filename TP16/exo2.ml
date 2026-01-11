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
  | N(g, e, _) -> g |> min_abr

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


(* Début exercice 2 *)
(* Nul *)
(* let rec supprime abr x =
  match abr with
  | V -> V
  | N (V, e, d) when e = x -> d
  | N (g, e, V) when e = x -> g
  | N (g, e, d) when e < x -> N(g, e, supprime d x)
  | N (g, e, d) when e > x -> N(supprime g x, e, d)
  | N (g, e, d) (* e = x *) ->
    let rec fusionner_abr abr acc =
      match abr with
      | V -> acc
      | N(g, e, d) ->
        acc
        |> insere e
        |> fusionner_abr g
        |> fusionner_abr d
    in
    fusionner_abr g d *)

let rec supprime abr x =
  match abr with
  | V -> V
  | N (V, e, d) when e = x -> d
  | N (g, e, V) when e = x -> g
  | N (g, e, d) when e < x -> N(g, e, supprime d x)
  | N (g, e, d) when e > x -> N(supprime g x, e, d)
  | N (g, e, d) (* e = x *) ->
    let rec mettre_a_gauche abr_g = function
      | V -> abr_g
      | N(g, e, d) -> N(mettre_a_gauche abr_g g, e, d)
    in
    mettre_a_gauche g d

let rec supprime2 abr x =
  match abr with
  | V -> V
  | N (V, e, d) when e = x -> d
  | N (g, e, V) when e = x -> g
  | N (g, e, d) when e < x -> N(g, e, supprime d x)
  | N (g, e, d) when e > x -> N(supprime g x, e, d)
  | N (g, e, d) (* e = x *) ->
    let rec supprime_min = function
      | V -> failwith "No min"
      | N(V, e, d) -> e, d
      | N(g, e, d) ->
        let m, g' = supprime_min g in
        m, N(g', e, d)
    in
    let m, d' = supprime_min d in
    N(g, m, d')

let () =
  assert (recherche (-4.8) exemple_3);

  let test_supprime supprime (valeur: 'a) (abr: 'a abr) =
    let abr = supprime abr valeur in
    assert (abr |> est_abr);
    assert (recherche (-4.8) abr |> not);
    assert (recherche (-6.) abr);
    assert (recherche (-5.1) abr);
    assert (recherche (10.5) abr)
  in

  test_supprime supprime (-4.8) exemple_3;
  test_supprime supprime2 (-4.8) exemple_3;

  assert (insere (-4.8) (supprime2 exemple_3 (-4.8)) = exemple_3)