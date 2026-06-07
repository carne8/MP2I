type queue =
    { mutable start: int;
      mutable end': int;
      mutable empty: bool;
      values: int array }

let create length =
    { start = 0;
      end' = 0;
      empty = true;
      values = Array.make length 0 }

let is_full queue = queue.start = queue.end'
let is_empty queue = queue.empty

let enqueue e queue =
    assert (queue.empty || queue |> is_full |> not);
    queue.empty <- false;
    queue.values.(queue.end') <- e;
    queue.end' <- (queue.end' + 1) mod (Array.length queue.values)

let dequeue queue =
    assert (queue |> is_empty |> not);
    let e = queue.values.(queue.start) in
    queue.start <- (queue.start + 1) mod (Array.length queue.values);
    queue.empty <- queue.start = queue.end';
    e

let () =
    let queue = create 10 in
    queue |> enqueue 0;
    queue |> enqueue 1;
    queue |> enqueue 2;
    queue |> enqueue 3;
    queue |> enqueue 18;
    queue |> enqueue 5;
    queue |> enqueue 6;
    queue |> enqueue 7;
    queue |> enqueue 8;
    queue |> enqueue 9;
    assert (queue |> is_full);

    for _ = 0 to 9 do
        queue |> dequeue |> Printf.printf "%i\n"
    done;
    assert (queue |> is_empty)