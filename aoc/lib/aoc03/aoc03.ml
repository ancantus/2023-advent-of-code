type part_num = {
        snipit: string;
        dim2: (int * int);
        mutable neighbours: part_num list;
}

let add_neighbour n1 n2 =
        n1.neighbours <- n2 :: n1.neighbours;
        n2.neighbours <- n1 :: n2.neighbours

let load_schematic channel = 
        let make_schematic str =
                let len = String.length str in
                Bigarray.Array2.create Bigarray.char Bigarray.c_layout len len in
        let first_line = Option.get (In_channel.input_line channel) in
        let schematic = make_schematic first_line in
        let rec load_impl i end_i str = 
                if i >= end_i then 
                        ()
                else 
                        let _ = String.iteri (fun j c -> Bigarray.Array2.set schematic i j c) str in
                        load_impl (i+1) end_i (Option.value (In_channel.input_line channel) ~default:"")
                in
        let _ = load_impl 0 (Bigarray.Array2.dim2 schematic) first_line in
        schematic

let is_decimal c =
        match c with '0'..'9' -> true | _ -> false

let find_pn slice =
        let parse_symbol idx =
                {
                        snipit = String.init 1 (fun _ -> slice.{idx});
                        dim2 = (idx, idx);
                        neighbours = [];
                } in
        let slice_max = Bigarray.Array1.dim slice in
        let parse_pn start_idx =
                let end_idx = ref start_idx in 
                let _ = while !end_idx < slice_max && is_decimal slice.{!end_idx} do
                        end_idx := !end_idx + 1 
                done in
                (!end_idx, {
                        snipit = String.init (!end_idx - start_idx) (fun i -> slice.{i + start_idx});
                        dim2 = (start_idx, !end_idx - 1);
                        neighbours = [];

                }) in
        let rec find_impl i part_nums = 
                if i >= slice_max then part_nums else 
                match slice.{i} with
                        | '.' -> find_impl (i+1) part_nums
                        | '0'..'9' -> let (next_idx, new_pn) = parse_pn i in
                                find_impl next_idx (new_pn :: part_nums)
                        | _ -> let new_pn = parse_symbol i in
                                find_impl (i+1) (new_pn :: part_nums)
                in
        let rec connect_line pn_list =
                let connect prev current = 
                        if (snd prev.dim2) + 1 == (fst current.dim2) 
                        then add_neighbour prev current
                        else ()
                in
                match pn_list with
                        | [] -> ()
                        | _ :: [] -> ()
                        | prev :: current :: rem -> connect prev current; connect_line (current :: rem)
        in
        let pn_list = List.rev (find_impl 0 []) in
        connect_line pn_list; 
        pn_list

let find_pn_graph schematic =
        let max_dim1 = Bigarray.Array2.dim1 schematic in
        let rec get_pn_generations dim1 generations = 
                if dim1 >= max_dim1 then
                       generations
                else
                        get_pn_generations (dim1 + 1)
                                ((find_pn (Bigarray.Array2.slice_left schematic dim1)) :: generations)
                in
        let pn_generations = List.rev (get_pn_generations 0 []) in
        let bind_generation ~prev:p ~next:n =
                let is_linked child_range parent_range =
                        let start = fst child_range - 1 in
                        let stop = snd child_range + 1 in
                        let in_range idx =
                               idx >= start && idx <= stop in
                        in_range (fst parent_range) || in_range (snd parent_range)
                in
                let bind_child child possible_parents =
                        let parents = List.filter 
                                (fun parent -> is_linked child.dim2 parent.dim2) possible_parents in
                        List.iter (fun i -> add_neighbour child i) parents
                in
                List.iter (fun child -> bind_child child p) n;
        in
        let rec bind_graph generations =
                match generations with
                        | [] -> ()
                        | _ :: [] -> ()
                        | parent_gen :: child_gen :: rem -> bind_generation ~prev:parent_gen ~next:child_gen;
                                bind_graph (child_gen :: rem)
        in
        bind_graph pn_generations;
        List.flatten pn_generations

let acc_part_num acc pn =
        let is_symbol str = match str.[0] with '.' -> false | '0'..'9' -> false | _ -> true in
        let symbol_adjacent = List.exists (fun i -> is_symbol i.snipit) pn.neighbours in
        if not (is_symbol pn.snipit) && symbol_adjacent then acc + (int_of_string pn.snipit) else acc

let run () =
        let schematic = load_schematic stdin in
        Printf.printf "Schematic Loaded: [%i, %i]\n" 
                (Bigarray.Array2.dim1 schematic) (Bigarray.Array2.dim2 schematic);
        let pn_graph = find_pn_graph schematic in
        let pn_sum = List.fold_left acc_part_num 0 pn_graph in
        Printf.printf "Part Number Sum: %i\n" pn_sum

