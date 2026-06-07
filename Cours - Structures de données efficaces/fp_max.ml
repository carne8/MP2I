type ('a, 'p) fp = ('a * 'p) list

let empty_fp = []
let est_vide : (_, _) fp -> bool = (=) []
let rec ajouter e p = function
  | [] -> [ e, p ]
  | (e', p')::tl when p > p' -> (e, p)::(e', p')::tl
  | (e', p')::tl -> (e', p')::ajouter e p tl

let retirer_max = function
  | [] -> failwith "Empty"
  | (e, p)::tl -> e, p