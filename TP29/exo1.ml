type valuation = bool array
type formule =
  | Top
  | Bot
  | Var of int
  | Not of formule
  | And of formule * formule
  | Or of formule * formule
  | Impl of formule * formule

let p =
  Impl(
    Or(Bot, Var 0),
    And(
      Or(
        And(Not(Var 1), Var 2),
        Var 0
      ),
      Impl(Top, Or(Var 2, Var 3))
    )
  )

let rec taille = function
  | Var _ | Top | Bot -> 1
  | Not f -> 1 + taille f
  | And(f1, f2)
  | Or(f1, f2)
  | Impl(f1, f2) -> 1 + taille f1 + taille f2

let rec ind_var_max = function
  | Top | Bot -> -1
  | Var i -> i
  | Not f -> ind_var_max f
  | And(f1, f2)
  | Or(f1, f2)
  | Impl(f1, f2) -> max (ind_var_max f1) (ind_var_max f2)

let rec valeur_veriter f v =
  match f with
  | Top -> true
  | Bot -> false
  | Var i -> v.(i)
  | Not f -> valeur_veriter f v |> not
  | And(f1, f2) -> valeur_veriter f1 v && valeur_veriter f2 v
  | Or(f1, f2) -> valeur_veriter f1 v || valeur_veriter f2 v
  | Impl(f1, f2) -> valeur_veriter f1 v |> not || valeur_veriter f2 v

let rec sub f k f' =
  match f with
  | Var i when i = k -> f'
  | Not f -> sub f k f'
  | And(f1, f2) -> And(sub f1 k f', sub f2 k f')
  | Or(f1, f2) -> Or(sub f1 k f', sub f2 k f')
  | Impl(f1, f2) -> Impl(sub f1 k f', sub f2 k f')
  | other -> other

let rec simplifier_formule = function (* O(|f|) *)
  | Not f -> begin
    match simplifier_formule f with
    | Top -> Bot
    | Bot -> Top
    | f -> Not f
  end
  | Or(f1, f2) -> begin
    match simplifier_formule f1, simplifier_formule f2 with
    | Top, _ | _, Top -> Top
    | Bot, f | f, Bot -> f
    | f1, f2 -> Or(f1, f2)
  end
  | And(f1, f2) -> begin
    match simplifier_formule f1, simplifier_formule f2 with
    | Bot, _ | _, Bot -> Bot
    | Top, f | f, Top -> f
    | f1, f2 -> And(f1, f2)
  end
  | Impl(f1, f2) -> Or(Not f1, f2) |> simplifier_formule
  | other -> other

type arbre_decision =
  | Feuille of bool
  | Noeud of arbre_decision * int * arbre_decision

let rec construire_arbre_decision f = (* O(4*|f|) *)
  f
  |> simplifier_formule
  |> function
    | Top -> Feuille true
    | Bot -> Feuille false
    | f ->
      let var_max = ind_var_max f in
      Noeud(
        sub f var_max Top |> construire_arbre_decision,
        var_max,
        sub f var_max Bot |> construire_arbre_decision
      )

let rec satisfaisable_quine f =
  let rec loop = function
    | Feuille r -> r
    | Noeud(f1, _, f2) -> loop f1 || loop f2
  in
  f
  |> construire_arbre_decision
  |> loop