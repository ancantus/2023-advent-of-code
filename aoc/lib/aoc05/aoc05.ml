type range = {
        start_src: int;
        start_dst: int;
        length: int;
}
type mapping = {
        name: string;
        mutable input: mapping option;
        mutable output: mapping option;
        ranges: range list;
}

let rec read_num_list channel l =
        let num = Scanf.bscanf_opt channel " %d " (fun i -> i) in
        match num with None -> l | Some n -> read_num_list channel (n :: l)

let read_seeds channel =
        Scanf.bscanf channel "seeds: " (fun () -> ()) ();
        read_num_list channel []

let read_seed_ranges channel =
        let seeds = read_seeds channel in
        let rec inner seeds ranges =
                match seeds with
                        | [] -> ranges
                        | s :: [] -> (s, 1) :: ranges
                        | l :: s :: tail -> inner tail ((s, l) :: ranges)
        in
        List.sort (fun a b -> compare (fst a) (fst b)) (inner seeds [])

let read_maps channel =
        let read_range channel =
                Scanf.bscanf_opt channel " %d %d %d " 
                        (fun start_dst start_src length -> {start_src; start_dst; length})
        in
        let read_ranges channel =
                let rec inner channel ranges =                
                        match read_range channel with
                                | None -> ranges
                                | Some r -> inner channel (r :: ranges)
                in
                inner channel []
        in
        let rec inner channel maps =
                let bind_maps maps = 
                        match maps with
                                | o :: i :: _ -> o.input <- Some i; i.output <- Some o; maps 
                                | _ -> maps
                in
                let header = Scanf.bscanf_opt channel "%s@-to%s map:" (fun input output -> (input, output)) in
                match header with
                        | None -> maps
                        | Some (input, _) -> inner channel (bind_maps ({
                                name=input;
                                input=None;
                                output=None;
                                ranges=read_ranges channel;
                        } :: maps))
        in
        let maps = inner channel [] in
        maps 

let map_lookup map key =
        let lookup_range key range =
                if key >= range.start_src && key < (range.start_src + range.length) 
                        then Some (range.start_dst + (key - range.start_src))
                        else None
        in
        let rec inner key ranges =
                match ranges with
                        | [] -> key
                        | r :: tail -> (match lookup_range key r with 
                                | Some k -> k 
                                | None -> inner key tail)
        in
        (map.output, inner key map.ranges)

let rec query_maps map key =
        match map with
                | None -> key
                | Some m -> let (m, key) = map_lookup m key in
                        query_maps m key

let make_seed_seq seed_ranges =
        let make_seq range =
                Seq.init (snd range) (fun i -> (fst range) + i)
        in
        let rec inner ranges sequence =
                match ranges with
                        | r :: tail -> inner tail (Seq.append sequence (make_seq r))
                        | _ -> sequence
        in
        inner seed_ranges Seq.empty

let run1 () =
        let seeds = read_seeds Scanf.Scanning.stdin in
        let maps = List.rev (read_maps Scanf.Scanning.stdin) in
        Printf.printf "Location maps:\n";
        List.iter (fun m -> Printf.printf "\t%s -> %s\n" m.name 
                (match m.output with None -> "location" | Some m -> m.name) ) maps;
        let min_location = List.fold_left 
                (fun acc s -> min acc (query_maps (Some (List.hd maps)) s)) max_int seeds in
        Printf.printf "Closest Location: %d\n" min_location

module T = Domainslib.Task
let parallel_min_loc pool maps seed_ranges =
        let min_locations = List.map (fun r -> T.async pool (fun _ -> 
                let seq = make_seed_seq [r] in
                Seq.fold_left 
                        (fun acc s -> min acc (query_maps (Some (List.hd maps)) s)) max_int seq)) seed_ranges in
        List.fold_left (fun acc l -> min acc (T.await pool l)) max_int min_locations

let run2 () = 
        let seed_ranges = read_seed_ranges Scanf.Scanning.stdin in
        let maps = List.rev (read_maps Scanf.Scanning.stdin) in
        Printf.printf "Location maps:\n";
        List.iter (fun m -> Printf.printf "\t%s -> %s\n" m.name 
                (match m.output with None -> "location" | Some m -> m.name) ) maps;
        let pool = T.setup_pool ~num_domains:15 () in
        let min_location = T.run pool (fun _ -> parallel_min_loc pool maps seed_ranges) in
        T.teardown_pool pool;
        Printf.printf "Closest Location: %d\n" min_location

