type ('a, 'p) fp = V | Node of ('a, 'p) fp * 'a * 'p * ('a, 'p) fp

let empty_fp = V
let est_vide = (=) V

(* let rec ajouter e p = function
  | V -> Node(V, e, p, V)
  | Node(g, e', p', d) when p < p' -> Node(ajouter e p g, e', p', d)
  | Node(g, e', p', d) when p > p' -> Node(g, e', p', ajouter e p d)
  | Node(g, e', p', d) when p = p' ->
    Node(g, e', p', d) *)

(* let retirer_max = function
  | [] -> failwith "Empty"
  | (e, p)::tl -> e, p *)