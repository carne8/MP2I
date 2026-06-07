type ('a, 'b) dict_abr =
  | Empty
  | Node of ('a, 'b) dict_abr * 'a * 'b * ('a, 'b) dict_abr

let rec mem_abr key = function
  | Empty -> false
  | Node (left, k, v, right) ->
    if key = k then true else 
    if key < k then 
      mem_abr key left
    else 
      mem_abr key right

let rec find_abr key = function
  | Empty -> raise Not_found
  | Node (left, k, v, right) ->
    if key = k then v else 
    if key < k then 
      find_abr key left
    else 
      find_abr key right

let rec find_abr_opt key = function
  | Empty -> None
  | Node (left, k, v, right) ->
    if key = k then Some v else 
    if key < k then 
      find_abr_opt key left
    else 
      find_abr_opt key right

let rec add_abr key value abr = 
  match abr with
  | Empty -> Node (Empty, key, value, Empty)
  | Node (left, k, v, right) ->
    if key = k then abr else 
    if key < k then 
      Node(add_abr key value left, k, v, right) 
    else 
      Node(left, k, v, add_abr key value right)

(* Ces fonctions sont en O(n) si les arbres ne sont pas equilibrés *)
(* Ces fonctions sont en O(log2 n) sinon *)