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
                Bigarray.Array2.init Bigarray.char Bigarray.c_layout len len (fun _ _ -> '.') in
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

let map_row_seq map =
        let dim1 = Bigarray.Array2.dim1 map in
        Seq.init dim1 (fun i -> Bigarray.Array2.slice_left map i)


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
                | 'I' -> None
                | 'O' -> None
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

let find_longest_loop start_pipe =
        let depth_first_search start =
                let rec inner visited_tbl pipe path =
                        let visited = Option.value (Coord_tbl.find_opt visited_tbl pipe.coords) ~default:false in
                        let is_start = pipe.coords = start.coords in
                        match is_start, visited with
                                | true, _ -> List.iter print_pipe (pipe :: path); print_endline "Some"; Some path 
                                | false, true -> List.iter print_pipe (pipe :: path); print_endline "None"; None 
                                | false, false -> Coord_tbl.add visited_tbl pipe.coords true;
                                        let possible_loops = List.filter_map 
                                                (fun p -> if p.coords = pipe.coords then None else
                                                        if p.coords = (List.hd path).coords then None else
                                                        inner visited_tbl p ( pipe :: path)) pipe.connections in
                                        let long_loop = List.fold_left 
                                                (fun acc l -> if List.length l > List.length acc 
                                                        then l else acc) [] possible_loops in
                                        if List.is_empty long_loop 
                                                then None
                                                else Some long_loop
                in
                List.find_map (fun p -> 
                        let visited_tbl = Coord_tbl.create 1 in inner visited_tbl p [start]) start.connections
        in
        depth_first_search start_pipe

let count_inclosed_tiles loop =
        let max_dims = List.fold_left (fun acc p -> match acc, p.coords with
                | (acc_x, acc_y), (p_x, p_y) -> (max acc_x p_x, max acc_y p_y)) (0, 0) loop in
        let wind_array = Bigarray.Array2.init 
                Bigarray.int Bigarray.c_layout ((fst max_dims) + 1) ((snd max_dims) + 1) (fun _ _ -> min_int) in
        let get_winding back p front = 
                let back_y = (fst p.coords) - (fst back.coords) in 
                if back_y > 0 then -1
                else if back_y < 0 then 1
                else let front_y = (fst p.coords) - (fst front.coords) in
                        if front_y > 0 then 1 
                        else if front_y < 0 then -1
                        else 0
        in
        let rec set_wind_array start pipes =
                match pipes with
                        | [] -> ()
                        | _ :: [] -> ()
                        | back :: p :: [] ->
                                        wind_array.{fst p.coords, snd p.coords} <- get_winding back p start;
                                        wind_array.{fst start.coords, snd start.coords} <- 
                                                get_winding p start (List.nth loop 1) 
                        | back :: p :: front :: tail -> 
                                        wind_array.{fst p.coords, snd p.coords} <- get_winding back p front;
                                        set_wind_array start (p :: front :: tail)
        in
        set_wind_array (List.hd loop) loop;
        Seq.iter (fun row -> for i = 0 to (Bigarray.Array1.dim row) - 1 do
                        if row.{i} = 1 then print_char 'U' 
                        else if row.{i} = -1 then print_char 'D'
                        else if row.{i} = 0 then print_char '-'
                        else print_char '.'
                  done; print_endline "" ) (map_row_seq wind_array);
        let rec inner_count row i last_cross winding count =
                let dim = Bigarray.Array1.dim row in
                if i >= dim then count 
                else if row.{i} = 0 then inner_count row (i+1) last_cross winding count
                else if row.{i} = last_cross then inner_count row (i+1) last_cross winding count 
                else if row.{i} = min_int then (
                        let count = if winding = 0 then count else count + 1 in
                        inner_count row (i+1) last_cross winding count
                )
                else inner_count row (i+1) row.{i} (winding + row.{i}) count
        in
        Seq.fold_left (fun tiles row -> tiles + (inner_count row 0 0 0 0)) 0 (map_row_seq wind_array)

let run () =
        let map = load_map stdin in
        let start_pipe = List.hd (make_pipe_graph map) in
        let longest_loop = List.rev (Option.value (find_longest_loop start_pipe) ~default:[]) in
        Printf.printf "Longest pipe distance: %d\n" ((List.length longest_loop) / 2);
        let inclosed_tile_count = count_inclosed_tiles longest_loop in
        Printf.printf "Inclosed tile count: %d\n" inclosed_tile_count  


