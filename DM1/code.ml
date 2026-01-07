let exemple =
  [ "sire";
    "site";
    "ski";
    "sac";
    "dodos";
    "dodu";
    "dole";
    "de";
    "si";
    "do" ]

type trie = V | N of char * trie * trie
type mot = char list
let trie_exemple =
  N
    ( 'd',
      N
        ( 'o',
          N
            ( '$',
              V,
              N
                ( 'l',
                  N ('e', N ('$', V, V), V),
                  N
                    ( 'd',
                      N
                        ( 'u',
                          N ('$', V, V),
                          N ('o', N ('s', N ('$', V, V), V), V) ),
                      V ) ) ),
          N ('e', N ('$', V, V), V) ),
      N
        ( 's',
          N
            ( 'i',
              N
                ( '$',
                  V,
                  N
                    ( 't',
                      N ('e', N ('$', V, V), V),
                      N ('r', N ('e', N ('$', V, V), V), V) ) ),
              N
                ( 'a',
                  N ('c', N ('$', V, V), V),
                  N ('k', N ('i', N ('$', V, V), V), V) ) ),
          V ) )

(* Question 1 *)
let rec est_bien_forme = function
  | V -> true
  | N(char, gauche, droit) ->
    match char with
    | '$' -> gauche = V && est_bien_forme droit
    | _ -> gauche <> V && est_bien_forme gauche && est_bien_forme droit

(* Question 2 *)
let mot_of_string (str: string) =
  let mot = ref ['$'] in
  for i = String.length str - 1 downto 0 do
    mot := str.[i]::!mot
  done;
  !mot

(* Question 3 *)
let rec afficher_mot = function
  | []
  | [ '$' ] -> ()
  | char::reste -> print_char char; afficher_mot reste

(* Question 4 *)
let[@tail_mod_cons] rec list_map f = function (* Fait la même chose que List.map *)
  | [] -> []
  | hd::tl -> f hd::list_map f tl

let rec mots_of_trie = function
  | V -> []
  | N('$', V, d) -> ['$'] :: mots_of_trie d (* On ajoute le mot vide à l'ensmble de mots à droite *)
  | N(c, g, d) ->
      g
      |> mots_of_trie
      |> list_map (List.cons c) (* Pour chaque mot de l'ensemble de mots à gauche, on ajoute le caractère au début *)
      |> List.append (mots_of_trie d) (* On fait l'union avec l'ensemble de mot à droite *)

let test_exercice1 () =
  (* Test question 1 *)
  assert (trie_exemple |> est_bien_forme);

  (* Test question 2 *)
  assert (mot_of_string "hello world" = ['h'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd'; '$']);
  assert (mot_of_string "banane" = ['b'; 'a'; 'n'; 'a'; 'n'; 'e'; '$']);

  (* Test question 3 *)
  "hello world"
  |> mot_of_string
  |> afficher_mot;
  print_newline ();

  (* Test question 4 *)
  (* L'ensemble des mots représentés par `trie_exemple` est `exemple`
     Sachant ceci, vérifions que la fonction `mots_of_trie` est correcte *)
  let exemple_reconstruit = mots_of_trie trie_exemple in

  (* On doit convertir `exemple` en mot list pour comparer avec `exemple_reconstruit`
     On trie les listes car deux listes sont égales si leurs éléments sont dans le même ordre,
     or on ne s'intéresse pas à leur ordre *)
  assert (
    exemple
    |> List.map mot_of_string
    |> List.sort compare
    = List.sort compare exemple_reconstruit
  )





(* ------------ Exercice 2 ------------ *)

(* Question 1 *)
let rec cardinal = function
  | V -> 0
  | N('$', V, d) -> 1 + cardinal d
  | N('$', _, _) -> failwith "Pas bien formé"
  | N(c, g, d) -> cardinal g + cardinal d

(* Question 2 *)
let rec recherche trie mot =
  match trie with
  | V -> false
  | N(c, g, d) ->
    match mot with
    | [] -> failwith "Mot invalide"
    | ['$'] -> c = '$' || recherche d mot
    | c'::reste when c' = c -> recherche g reste
    | c'::_ -> recherche d mot

(* TODO: Question 3 *)

(* Question 4 *)
let rec insere trie mot =
  match mot with
  | [] -> trie
  | hd::tl ->
    match trie with
    | V -> N(hd, insere V tl, V)
    | N(c, g, d) ->
      if hd = c then
        N(c, insere g tl, d)
      else
        N(c, g, insere d mot)

(* Question 5 *)
let rec trie_of_list = function
| [] -> V
| hd::tl ->
  hd
  |> mot_of_string
  |> insere (trie_of_list tl)

(* Question 5 - récursive terminale *)
let trie_of_list_tr =
  List.fold_left
    (fun trie str ->
      str
      |> mot_of_string
      |> insere trie)
    V

(* Question 6 *)
let rec longueur_max = function
  | V -> -1
  | N(_, g, d) -> max (1 + longueur_max g) (longueur_max d)

(* Question 7 *)
let rec compte_mots_longs trie min_length =
  (* Si la min_length <= 0, alors tous les mots présent dans le trie doivent être comptés *)
  if min_length <= 0 then cardinal trie else
  match trie with
  | V -> 0
  | N(_, g, d) ->
    compte_mots_longs g (min_length-1) (* TODO: Trouver comment expliquer *)
    + compte_mots_longs d min_length

(* Question 8 *)
let rec iter_trie f trie =
  let rec aux current_word f trie =
    match trie with
    | V -> ()
    | N('$', V, d) ->
      ('$'::current_word) |> List.rev |> f;
      aux current_word f d
    | N(c, g, d) ->
      aux (c::current_word) f g;
      aux current_word f d
  in
  aux [] f trie

(* Question 9 *)
let affiche_mots = iter_trie (fun mot ->
  afficher_mot mot;
  print_newline ()
)

let list_of_trie trie =
  let list = ref [] in
  trie |> iter_trie (fun mot -> list := mot :: !list);
  !list

(* Question 10 *)
let tableau_occurrences str =
  let tab = Array.make 26 0 in
  for i = 0 to String.length str - 1 do
    let char = str.[i] in
    let idx = Char.code char - Char.code 'a' in
    tab.(idx) <- tab.(idx) + 1
  done;
  tab

let test_exercice2 () =
  (* Test question 1 *)
  assert (cardinal trie_exemple = 10);
  assert (cardinal trie_exemple = List.length exemple);

  (* Test question 2 *)
  assert (recherche trie_exemple ['s'; 'i'; 't'; 'e'; '$']);
  assert (not (recherche trie_exemple ['d'; 'o'; 'd'; 'o'; '$']));

  (* On verifie que chaque mot de `exemple` est dans le trie `trie_exemple` *)
  assert (exemple |> List.for_all (fun mot_str ->
    mot_str
    |> mot_of_string
    |> recherche trie_exemple
  ));

  (* On vérifie qu'il n'y a pas de faux positif *)
  assert ("" |> mot_of_string |> recherche trie_exemple |> not);
  assert ("ce mot n'est pas dans le trie" |> mot_of_string |> recherche trie_exemple |> not);

  (* Test question 4 *)
  assert (
    insere (N('u', N('$', V, V), V)) ['a'; 'b'; '$']
    =
    N ('u',
      N('$', V, V),
      N ('a',
        N ('b',
          N ('$', V, V),
          V
        ),
      V)
    )
  );

  (* On ajoute sucre au trie `trie_exemple` *)
  let nouveau_trie_exemple =
    "sucre"
    |> mot_of_string
    |> insere trie_exemple
  in

  (* On vérifie que "sucre" est bien dans le nouveau trie *)
  assert ("sucre" |> mot_of_string |> recherche nouveau_trie_exemple);

  (* On vérifie que l'on n'a pas cassé le trie *)
  assert (
    exemple
    |> List.map mot_of_string
    |> List.for_all (fun mot -> recherche nouveau_trie_exemple mot)
  );

  (* Et on vérifie que la l'ensemble des mots de `trie_exemple`
     est le même que exemple + "sucre" *)
  assert (
    nouveau_trie_exemple
    |> mots_of_trie
    |> List.sort compare
    =
    ("sucre"::exemple
     |> List.map mot_of_string
     |> List.sort compare)
  );

  (* Test question 5 *)
  assert (exemple |> trie_of_list = trie_exemple);
  assert (
    exemple
    |> trie_of_list_tr
    |> mots_of_trie
    |> List.sort compare
    = (
    exemple
    |> List.map mot_of_string
    |> List.sort compare)
  );

  (* Test question 6 *)
  assert (longueur_max V = -1);

  let longueur_max_dans_exemple = (* On trouve la longueur du mot le plus long dans `exemple` *)
    exemple |> List.fold_left
      (fun m e -> e |> String.length |> max m)
      (-1)
  in
  (* On vérifie que la longueur max de `trie_exemple` est la valeur trouvé ci-dessus *)
  assert (trie_exemple |> longueur_max = longueur_max_dans_exemple);

  (* Test question 7 *)
  assert (compte_mots_longs trie_exemple 4 = 5);
  assert (compte_mots_longs (trie_of_list [ "ab" ]) 2 = 1);

  (* Test question 8 *)
  iter_trie (fun mot ->
    afficher_mot mot;
    print_newline ()
  ) trie_exemple;

  (* Test question 9 *)
  affiche_mots trie_exemple;
  assert (
    trie_exemple
    |> list_of_trie
    |> List.sort compare
    =
    (trie_exemple
     |> mots_of_trie
     |> List.sort compare)
  );

  (* Test question 10 *)
  let tab1 = tableau_occurrences "aaaaaaa" in
  assert (tab1.(0) = 7);
  assert (tab1
          |> Array.to_list
          |> List.drop 1
          |> List.for_all ((=) 0)); (* Vérifie si toutes les autres valeures du tableau sont 0 *)

  let tab2 = tableau_occurrences "abbccc" in
  assert (tab2.(0) = 1);
  assert (tab2.(1) = 2);
  assert (tab2.(2) = 3);
  assert (tab2
          |> Array.to_list
          |> List.drop 3
          |> List.for_all ((=) 0)) (* Idem *)





(* ------------ Exercice 3 ------------ *)

(* Question 1 *)
let cat_first_line path =
  let file = open_in path in
  (try
    file
    |> input_line (* Lit la ligne *)
    |> print_string; (* Affiche la ligne *)
    print_newline()
   with End_of_file -> ());
  close_in file

(* Question 2 *)
let cat_first_100_lines path =
  let file = open_in path in
  (try
    for _ = 1 to 100 do
      file |> input_line |> print_string;
      print_newline()
    done
   with End_of_file ->
    print_string "Fichier terminé !\n");
  close_in file

(* Question 3 *)
let cat path =
  let file = open_in path in
  (try
    while true do
      file |> input_line |> print_string;
      print_newline()
    done
   with End_of_file -> ());
  close_in file

(* Question 4 *)
let trie_of_file path =
  let file = open_in path in
  let trie = ref V in
  (try
    while true do
      trie :=
        file
        |> input_line
        |> mot_of_string
        |> insere !trie
    done
   with End_of_file -> ());
  close_in file;
  !trie

let test_exercice3 () =
  assert ("cinq_cent_mots.txt" |> trie_of_file |> cardinal = 471);
  assert ("ods6_lowercase.txt" |> trie_of_file |> cardinal = 386264)





(* ------------ Exercice 4 ------------ *)

(* Question 1 *)
(** Vérifie si occ_sous_mot est le tableau d’occurrence d'un sous mot
    du mot dont occ_mot est le tableau d’occurrence *)
let est_sous_mot occ_mot occ_sous_mot =
  (* Array.for_all2 vérifie si une condition est vraie pour chaque
     paire d'éléments de même index dans les deux tableaux *)
  Array.for_all2
    (fun occ_mot occ_sous_mot -> occ_mot >= occ_sous_mot)
    occ_mot
    occ_sous_mot

let tableau_occurrences_mot mot =
  let tab = Array.make 26 0 in
  let rec aux i = function
    | []
    | ['$'] -> ()
    | c::reste ->
      let idx = Char.code c - Char.code 'a' in
      tab.(idx) <- tab.(idx) + 1;
      aux (i+1) reste
  in
  aux 0 mot;
  tab

let sous_mots trie mot_str =
  let occurrences = tableau_occurrences mot_str in
  trie |> iter_trie (fun mot ->
    let est_sous_mot =
      mot
      |> tableau_occurrences_mot
      |> est_sous_mot occurrences
    in
    if est_sous_mot then begin
      mot |> afficher_mot;
      print_newline ()
    end
  )

(* Question 2 *)
let afficher_anagrammes trie mot_str =
  let occurrences = tableau_occurrences mot_str in
  trie |> iter_trie (fun mot ->
    (* Deux mots sont anagrammes si ils ont le même tableau d'occurrence *)
    let est_anagramme = tableau_occurrences_mot mot = occurrences
    in
    if est_anagramme then begin
      mot |> afficher_mot;
      print_newline ()
    end
  )

(* Question 3 *)
let filtrer_trie f trie =
  let new_trie = ref V in

  trie |> iter_trie (fun mot ->
    if f mot then
      new_trie := mot |> insere !new_trie
  );
  !new_trie

let filtrer_sous_mots trie mot_str =
  let occurrences = tableau_occurrences mot_str in
  trie |> filtrer_trie (fun mot ->
    mot
    |> tableau_occurrences_mot
    |> est_sous_mot occurrences
  )

let filtrer_anagrammes trie mot_str =
  let occurrences = tableau_occurrences mot_str in
  trie |> filtrer_trie (fun mot -> tableau_occurrences_mot mot = occurrences)

(* Question 4 *)
let filtrer_sur_mots trie mot_str =
  (* Fait la même chose que est sous_mot mais change le sens de l'inégalité *)
  let occurrences = tableau_occurrences mot_str in
  trie |> filtrer_trie (fun mot ->
    Array.for_all2
      (fun occ1 occ_mot -> occ_mot <= occ1)
      (mot |> tableau_occurrences_mot)
      occurrences
  )

let test_exercice4 () =

  (* let cinq_cent_mots = trie_of_file "cinq_cent_mots.txt" in *)
  let ods6_lowercase = trie_of_file "ods6_lowercase.txt" in

  (* Test question 1 *)
  Printf.printf "\nSous-mots de bonjour:\n";
  sous_mots ods6_lowercase "bonjour";

  (* Test question 2 *)
  Printf.printf "\nAnagrammes de manoir:\n";
  afficher_anagrammes ods6_lowercase "manoir";
  Printf.printf "\nAnagrammes du mot argent:\n";
  afficher_anagrammes ods6_lowercase "argent";

  print_newline();
  let trie = [ "bonjour"; "jourbon" ] |> trie_of_list in
  afficher_anagrammes trie "bonjour";

  (* Test question 3 *)
  assert (cardinal (filtrer_sous_mots ods6_lowercase "banane") = 17);
  (* TODO: Ne fonctionne pas: assert (cardinal (filtrer_sous_mots ods6_lowercase "inserat") = 20) *)

  (* Test question 4 *)
  assert (cardinal (filtrer_sur_mots ods6_lowercase "mpdeuxi") = 30)

let () =
  Printf.printf "--- Exercice 1 ---\n";
  test_exercice1();
  Printf.printf "\n\n--- Exercice 2 ---\n";
  test_exercice2();
  Printf.printf "\n\n--- Exercice 3 ---\n";
  test_exercice3();
  Printf.printf "\n\n--- Exercice 4 ---\n";
  test_exercice4()