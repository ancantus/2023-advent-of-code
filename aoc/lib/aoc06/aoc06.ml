let read_num_list channel ~label:label =
        let rec inner_read channel l =
                let num = Scanf.bscanf_opt channel " %d " (fun i -> i) in
                        match num with None -> l | Some n -> inner_read channel (n :: l) in
        match label with
                | None -> inner_read channel []
                | Some l -> (match Scanf.bscanf_opt channel " %s@:" (fun s -> s) with
                        | None -> []
                        | Some s -> if s <> l then [] else inner_read channel [])


let quadratic a b c =
        let inner sign a b c =
                (sign (-1.0 *. b) (sqrt ( (b *. b) -. (4.0 *. a *. c)))) /. (2.0 *. a)
        in
        (inner ( +. ) a b c, inner ( -. ) a b c)

let downcasting_op f v =
        int_of_float (f v)
let casting_ceil = downcasting_op Float.ceil
let casting_floor = downcasting_op Float.floor

(* The race margin ends up being `dist = (time * x) - x^2` where x is the hold time *)
let calc_race_margin time best_dist =
        let dist = best_dist + 1 in (* only have to win by 1mm *)
        let (t_min, t_max) = quadratic (-1.0) (float_of_int time) (float_of_int (-1 * dist)) in
        (casting_ceil t_min, casting_floor t_max)

let run() =
        let times = read_num_list Scanf.Scanning.stdin ~label:(Some "Time") in
        let distances = read_num_list Scanf.Scanning.stdin ~label:(Some "Distance") in
        let margins = List.map2 calc_race_margin times distances in
        let combined_margins = List.fold_left (fun acc (m1, m2) -> acc * ((m2 - m1) + 1)) 1 margins in
        Printf.printf "Combined Race Margins: %d\n" combined_margins

