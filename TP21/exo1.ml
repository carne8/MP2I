(* O(n) *)
let rec find_assoc_opt key = function
  | [] -> raise Not_found
  | (k, v)::tl when k = key -> v
  | _::tl -> find_assoc_opt key tl
let find_assoc key l =
  l 
  |> List.find_opt (fun (k, _) -> k = key)
  |> function
    | None -> raise Not_found
    | Some e -> e

(* O(n) *)
let find_assoc_opt key = List.find_opt (fun (k, _) -> k = key)
let rec find_assoc_opt key = function
  | [] -> None
  | (k, v)::tl when k = key -> Some v
  | _::tl -> find_assoc_opt key tl

(* O(n) *)
let mem_assoc key = List.exists (fun (k, _) -> k = key)
let rec mem_assoc key = function
  | [] -> false
  | (k, _)::tl -> key = k || mem_assoc k tl

(* O(n) *)
let rec remove_assoc key = function 
  | [] -> []
  | (k, v)::tl when k = key -> tl
  | _::tl -> remove_assoc key tl