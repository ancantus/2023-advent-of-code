type pipe = {
        coords : int * int;
        symbol : char;
        mutable visited : bool;
        mutable connections : pipe list;
}

module Coord_tbl = Hashtbl.Make(struct
        type t = int * int
        let equal = ( = )
        let hash = (fun coords -> Hashtbl.hash coords)
end)

let print_pipe p =
        Printf.printf " %c: (%d, %d) " p.symbol (fst p.coords) (snd p.coords)

let add_pipe pipe_tbl p =
        Coord_tbl.add pipe_tbl p.coords p

let load_map channel =
        let make_map str =
                let len = String.length str in
                Bigarray.Array2.create Bigarray.char Bigarray.c_layout len len in
        let first_line = Option.get (In_channel.input_line channel) in
        let map = make_map first_line in
        let rec load_impl i end_i str =
                if i >= end_i then
                        ()
                else
                        let _ = String.iteri (fun j c -> Bigarray.Array2.set map i j c) str in
                        load_impl (i+1) end_i (Option.value (In_channel.input_line channel) ~default:"")
                in
        let _ = load_impl 0 (Bigarray.Array2.dim2 map) first_line in
        map

let map_coords_seq map =
        let dim1 = Bigarray.Array2.dim1 map in
        let dim2 = Bigarray.Array2.dim2 map in
        Seq.init (dim1 * dim2) (fun i -> (i / dim1, i mod dim2))

let in_map_range map coords =
        let dim1 = Bigarray.Array2.dim1 map in
        let dim2 = Bigarray.Array2.dim2 map in
        if (fst coords) < 0 || (fst coords) >= dim1 then
                false
        else if (snd coords) < 0 || (snd coords) >= dim2 then
                false
        else true

let make_pipe symbol_map coords =
        if not (in_map_range symbol_map coords) then None else
        let symbol = symbol_map.{fst coords, snd coords} in
        match symbol with
                | '.' -> None
                | _ -> Some { coords; symbol; visited=false; connections=[]}

let get_pipe symbol_map pipes coords = 
        match Coord_tbl.find_opt pipes coords with
                | Some p -> Some p
                | None -> (match make_pipe symbol_map coords with 
                        | None -> None | Some p -> add_pipe pipes p; Some p)

let bind_pipe symbol_map pipes p =
        let bind p1 p2 =
                p1.connections <- p2 :: p1.connections;
                if p2.symbol = 'S' then p2.connections <- p1 :: p2.connections else ()
        in
        let bind_prev pipes offset p =
                let found_p = Coord_tbl.find_opt pipes 
                        ((fst p.coords) - (fst offset), (snd p.coords) - (snd offset)) in
                match found_p with 
                        | None -> ()
                        | Some prev_p -> bind p prev_p
        in
        let bind_next symbol_map pipes offset p =
                let next_p = get_pipe symbol_map pipes 
                        ((fst p.coords) + (fst offset), (snd p.coords) + (snd offset)) in
                match next_p with 
                        | None -> ()
                        | Some new_p -> bind p new_p
        in
        let bind_north = bind_prev pipes (1, 0) in
        let bind_west = bind_prev pipes (0, 1) in
        let bind_south = bind_next symbol_map pipes (1, 0) in
        let bind_east = bind_next symbol_map pipes (0, 1) in
        match p.symbol with
                | '|' -> bind_north p; bind_south p
                | '-' -> bind_west p; bind_east p
                | 'L' -> bind_north p; bind_east p 
                | 'J' -> bind_north p; bind_west p
                | '7' -> bind_south p; bind_west p
                | 'F' -> bind_south p; bind_east p
                | 'S' -> () (* if we bind to nothing: only connected pipes will bind up *)
                | _ -> failwith ("Unhandled Symbol " ^ (String.make 1 p.symbol))

let make_pipe_graph symbol_map =
        let dim1 = Bigarray.Array2.dim1 symbol_map in
        let dim2 = Bigarray.Array2.dim2 symbol_map in
        let pipes = Coord_tbl.create (dim1 * dim2) in
        let make_graph_node coords = 
                match get_pipe symbol_map pipes coords with
                        | None -> None
                        | Some p -> bind_pipe symbol_map pipes p; Some p
        in
        let pipe_graph = Seq.map make_graph_node (map_coords_seq symbol_map) in
        Seq.fold_left (fun start_pipes p -> match p with 
                | None -> start_pipes 
                | Some p -> if p.symbol = 'S' then p :: start_pipes else start_pipes)
                [] pipe_graph

let find_longest_loop start_pipes =
        let next_pipes start =
                List.filter_map (fun p -> if (start.coords = p.coords) || p.visited
                        then None else Some p) start.connections
        in
        let rec count_loops pipes steps loop_counts =
                let next_pipes = List.fold_left (fun acc p -> (next_pipes p) @ acc) [] pipes in
                List.iter (fun p -> p.visited <- true) next_pipes;
                match next_pipes with
                        | _ :: [] -> loop_counts 
                        | [] -> loop_counts
                        | head :: tail -> if List.for_all (fun p -> head.coords = p.coords) tail 
                                then (steps + 1) :: loop_counts
                                else count_loops next_pipes (steps + 1) loop_counts
        in
        List.iter (fun p -> p.visited <- true) start_pipes;
        let loop_counts = count_loops start_pipes 0 [] in
        List.fold_left max 0 loop_counts

let run () =
        let map = load_map stdin in
        let start_pipes = make_pipe_graph map in
        let loop_count = find_longest_loop start_pipes in
        Printf.printf "Longest pipe distance: %d\n" loop_count


