type deque =
    { mutable start: int;
      mutable end': int;
      mutable empty: bool;
      values: int array }

let (%%%) x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y


let create length =
    { start = 0;
      end' = 0;
      empty = true;
      values = Array.make length 0 }

let is_full deque = deque.start = deque.end'
let is_empty deque = deque.empty

let push_right e deque =
    assert (deque.empty || deque |> is_full |> not);
    deque.empty <- false;
    deque.values.(deque.end') <- e;
    deque.end' <- (deque.end' + 1) %%% (Array.length deque.values)

let push_left e deque =
    assert (deque.empty || deque |> is_full |> not);
    deque.empty <- false;
    deque.start <- (deque.start - 1) %%% (Array.length deque.values);
    deque.values.(deque.start) <- e

let pop_left deque =
    assert (deque |> is_empty |> not);
    let e = deque.values.(deque.start) in
    deque.start <- (deque.start + 1) %%% (Array.length deque.values);
    deque.empty <- deque.start = deque.end';
    e

let pop_right deque =
    assert (deque |> is_empty |> not);
    deque.end' <- (deque.end' - 1) %%% (Array.length deque.values);
    deque.empty <- deque.start = deque.end';
    deque.values.(deque.end')

let () =
    let deque = create 6 in
    deque |> push_left (-1);
    deque |> push_left (-2);
    deque |> push_left (-3);

    deque |> push_right 1;
    deque |> push_right 2;
    deque |> push_right 3;
    assert (deque |> is_full);

    try
        deque |> push_left 69;
        assert false
    with _ -> ();

    assert (deque |> pop_left = -3);
    assert (deque |> pop_right = 3);
    assert (deque |> pop_left = -2);
    assert (deque |> pop_right = 2);
    assert (deque |> pop_left = -1);
    assert (deque |> pop_right = 1);
    assert (deque |> is_empty)