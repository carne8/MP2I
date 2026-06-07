type 'a element =
    { value: 'a;
      mutable next: 'a element option }

type 'a file =
    { mutable head: 'a element option;
      mutable tail: 'a element option }

let create () = { head = None; tail = None }

let enqueue x file =
    begin
    match file.tail with
    | None -> file.tail <- Some { value = x; next = None }
    | Some t ->
        t.next <- Some { value = x; next = None };
        file.tail <- t.next
    end;
    match file.head with
    | None -> file.head <- file.tail
    | _ -> ()

let dequeue file =
    match file.head with
    | None -> failwith "Empty"
    | Some e ->
        file.head <- e.next;
        e.value

let () =
    let queue = create () in
    queue |> enqueue 0;
    queue |> enqueue 1;
    queue |> enqueue 2;
    queue |> enqueue 3;
    queue |> enqueue 4;
    queue |> enqueue 5;
    queue |> enqueue 6;
    queue |> enqueue 7;
    queue |> enqueue 8;
    queue |> enqueue 9;

    for i = 0 to 9 do
        assert (queue |> dequeue = i)
    done
