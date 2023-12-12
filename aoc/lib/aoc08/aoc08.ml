let read_instructions channel =
        match Scanf.bscanf_opt channel " %s " (fun s -> s) with
                | None -> Seq.empty
                | Some s -> Seq.init (String.length s) 
                (fun i -> if s.[i] = 'L' then 0 else 1) |> Seq.cycle

let read_network channel =
        let rec inner_read channel =
                match Scanf.bscanf_opt channel "%s = (%s@, %s@)\n" 
                        (fun label left right -> (label, left, right)) with
                        | None -> []
                        | Some e -> e :: inner_read channel
        in
        let networks = inner_read channel |> List.fast_sort (fun (l1, _, _) (l2, _, _) -> compare l1 l2) in
        (* Map the label pointers into a single array for fast jumping *)
        let keys = Hashtbl.create (List.length networks) in
        List.iteri (fun i (label, _, _) -> Hashtbl.add keys label i) networks;
        let network = Bigarray.Array1.create Bigarray.int Bigarray.c_layout 
                ((List.length networks) * 2) in
        List.iteri (fun i (_, left, right) -> 
                network.{i*2} <- Hashtbl.find keys left;
                network.{(i*2) + 1} <- Hashtbl.find keys right) networks;
        network

let navigate_network network instructions =
        let end_idx = (Bigarray.Array1.dim network / 2) - 1 in (* landing on either leg is fine *)
        let rec inner_nav network endi i instructions =
                if i >= endi then 0
                else (match instructions () with 
                        | Seq.Nil -> 0
                        | Seq.Cons(off, tail) -> 1 + (inner_nav network endi network.{(i*2)+off} tail))
        in
        inner_nav network end_idx 0 instructions

let run () =
        let instructions = read_instructions Scanf.Scanning.stdin in
        Seq.take 20 instructions |> Seq.iter (fun i -> Printf.printf "%s" (if i = 0 then "L" else "R"));
        print_endline "";
        let network = read_network Scanf.Scanning.stdin in
        for i = 0 to ((Bigarray.Array1.dim network) / 2) - 1 do
                Printf.printf "%d -> (%d, %d)\n" i network.{i*2} network.{(i*2)+1}
        done;
        Printf.printf "Num Steps: %d\n" (navigate_network network instructions)

