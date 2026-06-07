(* Q1 = 3 14 + 5 2 - X = (3+14) X (5-2) = 17 * 3 = 51 *)
(* Q2 = 7 9 - 2 X 3 + 6 X *)
type operateur = Plus | Moins | Mult | Div
type expr_postfix =
  | Val of int
  | Op of operateur

let e1 = [ Val 3; Val 14; Op Plus; Val 5; Val 2; Op Moins; Op Mult ]
let e2 = [ Val 7; Val 9; Op Moins; Val 2; Op Mult; Val 3; Op Plus; Val 6; Op Mult ]

let apply_op op v1 v2 =
  match op with
  | Plus -> v1 + v2
  | Moins -> v1 - v2
  | Mult -> v1 * v2
  | Div -> v1 / v2

let evalue_postfixe expr =
  let s = Stack.create() in
  expr |> List.iter (function 
    | Val e -> Stack.push e s
    | Op op -> begin
      let b = Stack.pop_opt s in
      let a = Stack.pop_opt s in
      match a, b with
      | Some a, Some b -> Stack.push (apply_op op a b) s
      | _ -> failwith "Not enough parameters"
    end
  );
  match Stack.pop_opt s with
  | None -> failwith "Empty expr"
  | Some e -> e

type expr_infixe =
  | V of int
  | O of operateur
  | Par_ouvr
  | Par_ferm

let bien_parenthesee expr =
  let rec aux l_par_allowed r_par_allowed expect_closing = function
    | [] -> expect_closing = 0

    | Par_ouvr::tl when l_par_allowed -> aux true false (expect_closing + 1) tl
    | Par_ferm::tl when r_par_allowed -> 
      if expect_closing = 0 then false
      else aux false true (expect_closing - 1) tl
    | Par_ferm::_ | Par_ouvr::_ -> false
    
    | O _::tl -> aux true false expect_closing tl
    | V _::tl -> aux true true expect_closing tl
  in
  aux true true 0 expr

let stack_to_list stack =
  let rec aux acc =
    match Stack.pop_opt stack with
    | None -> acc
    | Some e -> aux (e::acc)
  in
  aux []

let infixe_to_postfix expr =
  let p1 = Stack.create() in
  let p2 = Stack.create() in

  expr |> List.iter (fun e ->
    match e with
    | V i -> Stack.push e p2
    | O op -> Stack.push e p1
    | Par_ouvr -> Stack.push e p1
    | Par_ferm -> begin
      match Stack.pop_opt p1, Stack.pop_opt p1 with
      | Some O op, Some Par_ouvr -> Stack.push (O op) p2
      | Some Par_ouvr, None -> ()
      | Some Par_ouvr, Some o -> Stack.push o p1
      | a, b -> failwith "Errorr"
    end
  );

  let rec aux acc =
    match Stack.pop_opt p2 with
    | None -> acc
    | Some Par_ouvr
    | Some Par_ferm -> failwith "Error"
    | Some O o -> aux (Op o::acc)
    | Some V e -> aux (Val e::acc)
  in
  aux []

let () =
  [ Par_ouvr; Par_ouvr; V 1; Par_ferm; O Plus; Par_ouvr; V 2; Par_ferm; Par_ferm ]
  |> infixe_to_postfix
  |> ignore