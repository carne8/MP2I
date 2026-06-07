type 'a ag = N of 'a * 'a ag list

let parcours_largeur f a =
  let queue = Queue.create() in
  Queue.push a queue;
  while queue |> Queue.is_empty |> not do
    let (N (e, children)) = Queue.pop queue in
    f e;
    children |> List.iter (fun a -> Queue.push a queue)
  done

let () =
  N(0, [
    N(1, [
      N(6, [])
    ]);
    N(2, [
      N(7, [
        N(8, [])
      ])
    ]);
    N(3, []);
    N(4, []);
    N(5, [])
  ])
  |> parcours_largeur (Printf.printf "%i\n")