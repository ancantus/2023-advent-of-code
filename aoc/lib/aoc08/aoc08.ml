let read_instructions channel =
        match Scanf.bscanf_opt channel " %s " (fun s -> s) with
                | None -> Seq.empty
                | Some s -> Seq.init (String.length s) 
                (fun i -> if s.[i] = 'L' then 1 else 2) |> Seq.cycle

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
        let node_type label = match label.[(String.length label) - 1] with 'A' -> 1 | 'Z' -> -1 | _ -> 0 in 
        let network = Bigarray.Array1.create Bigarray.int Bigarray.c_layout 
                ((List.length networks) * 3) in
        List.iteri (fun i (label, left, right) -> 
                network.{i*3} <- node_type label;
                network.{(i*3) + 1} <- Hashtbl.find keys left;
                network.{(i*3) + 2} <- Hashtbl.find keys right) networks; 
        network

let navigate_network ?(start_node=0) network instructions is_end =
        let rec inner_nav network i instructions steps =
                if is_end i then steps
                else (match instructions () with 
                        | Seq.Nil -> steps
                        (* tailcall annotation makes sure this is optimized *)
                        | Seq.Cons(off, tail) -> 
                                ((inner_nav [@tailcall]) network network.{(i*3)+off} tail (steps+1)))
        in
        inner_nav network start_node instructions 0

let run1 () =
        let instructions = read_instructions Scanf.Scanning.stdin in
        Seq.take 20 instructions |> Seq.iter (fun i -> Printf.printf "%s" (if i = 0 then "L" else "R"));
        print_endline "";
        let network = read_network Scanf.Scanning.stdin in
        for i = 0 to ((Bigarray.Array1.dim network) / 3) - 1 do
                Printf.printf "%d -> (%d, %d)\n" i network.{(i*3)+1} network.{(i*3)+1}
        done;
        let end_idx = (Bigarray.Array1.dim network / 3) - 1 in (* landing on either leg is fine *)
        Printf.printf "Num Steps: %d\n" (navigate_network network instructions (fun i -> i >= end_idx))

let rec find_nodes f network maxi i =
        if i >= maxi then []
        else (if f network.{i*3} then [i] else []) @ find_nodes f network maxi (i+1)
let find_start_nodes network = find_nodes (fun x -> x = 1) network ((Bigarray.Array1.dim network) / 3) 0
let find_end_nodes network = find_nodes (fun x -> x = -1) network ((Bigarray.Array1.dim network) / 3) 0

let rec gcd nums =
        match nums with
                | u :: [] -> u 
                | u :: 0 :: tail -> gcd ((abs u) :: tail)
                | u :: v :: tail -> gcd (v :: (u mod v) :: tail)
                | [] -> 0

let rec lcm nums =
        match nums with
                | u :: [] -> u
                | [] -> 0
                | u :: v :: tail -> lcm (((u * v) / (gcd [u; v])) :: tail)

let run2 () =
        let instructions = read_instructions Scanf.Scanning.stdin in
        Seq.take 20 instructions |> Seq.iter (fun i -> Printf.printf "%s" (if i = 1 then "L" else "R"));
        print_endline "";
        let network = read_network Scanf.Scanning.stdin in
        for i = 0 to ((Bigarray.Array1.dim network) / 3) - 1 do
                Printf.printf "%d -> (%d, %d)\n" i network.{(i*3)+1} network.{(i*3)+1}
        done;
        let start_nodes = find_start_nodes network in
        let path_lengths = List.map (fun node -> navigate_network ~start_node:node network instructions 
                (fun i -> network.{i*3} = -1)) start_nodes in
        List.iter (Printf.printf "Path Length: %d\n") path_lengths;
        Printf.printf "Min total path: %d\n" (lcm path_lengths)

