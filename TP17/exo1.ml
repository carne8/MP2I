let tri_1 = List.sort (fun x y -> abs x - abs y)
let tri_2 = List.sort (fun (a, b) (a', b') ->
  abs a - abs a'
  |> function
    | 0 -> compare b b'
    | o -> o
)

let rec compare_list l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> -1
  | _::tl1, _::tl2 -> compare_list tl1 tl2

let tri_3 = List.sort compare_list (* O(n*log(n) * min(|l1|, |l2|)) *)
let tri_3_mieux lists =
  lists
  |> List.map (fun l -> List.length l, l)
  |> List.sort (fun (x, _) (y, _) -> x - y)
  |> List.map (fun (_, l) -> l)

let () =
  assert (List.sort (-) [7; 1; 3; 2; 0] = [0; 1; 2; 3; 7]);
  assert (List.sort (fun x y -> x - y) [7; 1; 3; 2; 0] = [0; 1; 2; 3; 7]);
  assert (List.sort (fun x y -> y - x) [7; 1; 3; 2; 0] = [7; 3; 2; 1; 0]);

  assert (tri_1 [7; 1; 3; 2; 0; -18] = [0; 1; 2; 3; 7; -18]);

  let l1 = [1] in
  let l2 = [2;2] in
  let l3 = [1;1;1;1;1] in
  assert (tri_3 [l3;l2;l1] = [l1;l2;l3]);
  assert (tri_3_mieux [l3;l2;l1] = [l1;l2;l3])