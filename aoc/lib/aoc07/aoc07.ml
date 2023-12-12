type hand =
        | FiveKind of string
        | FourKind of string
        | Full of string
        | ThreeKind of string
        | TwoPair of string
        | OnePair of string
        | High of string

let hand_to_string h =
        match h with 
                | FiveKind s -> "Five of a kind: " ^ s
                | FourKind s -> "Four of a kind: " ^ s
                | Full s -> "Full house: " ^ s
                | ThreeKind s -> "Three of a kind: " ^ s
                | TwoPair s -> "Two pairs: " ^ s
                | OnePair s -> "One pair: " ^s
                | High s -> "High card: " ^s

let get_hand_string h = 
        match h with 
                | FiveKind s -> s
                | FourKind s -> s
                | Full s -> s
                | ThreeKind s -> s
                | TwoPair s -> s
                | OnePair s -> s
                | High s -> s

(* Map the non-numeric cards around the 2..9 ascii char space *)
let card_to_int c =
        match c with
                | 'X' -> 47
                | '2'..'9' -> int_of_char c
                | 'T' -> 58
                | 'J' -> 59
                | 'Q' -> 60
                | 'K' -> 61
                | 'A' -> 62
                | _ -> failwith "Unknown card"

let compare_hand lhs rhs =
        let class_order h =
                match h with
                        | FiveKind _ -> 6
                        | FourKind _ -> 5
                        | Full _ -> 4
                        | ThreeKind _ -> 3
                        | TwoPair _ -> 2
                        | OnePair _ -> 1
                        | High _ -> 0
        in
        let compare_card lhs rhs = Int.compare (card_to_int lhs) (card_to_int rhs) in
        let rec hand_str_compare lhs rhs i =
                if i >= String.length lhs then 0
                else 
                        let card_compare = compare_card lhs.[i] rhs.[i] in
                        if card_compare <> 0 then card_compare
                        else hand_str_compare lhs rhs (i+1)
        in
        let class_compare = Int.compare (class_order lhs) (class_order rhs) in
        if class_compare <> 0 
                then class_compare
                else hand_str_compare (get_hand_string lhs) (get_hand_string rhs) 0

let make_hand str =
        let stripped_hand = String.fold_left 
                (fun acc c -> if c = 'X' then acc else acc ^ (Char.escaped c)) "" str in 
        let num_jokers = (String.length str) -(String.length stripped_hand) in
        let hand_arr = Array.init (String.length stripped_hand) (fun i -> stripped_hand.[i]) in
        Array.fast_sort Char.compare hand_arr;
        let deduce_hand repeats unpaired =
                match repeats with
                        (* Joker deletion doesn't work if there are no pairs to glob onto *)
                        | 0 -> (match num_jokers with
                                        | 0 -> High str
                                        | 1 -> OnePair str
                                        | 2 -> ThreeKind str
                                        | 3 -> FourKind str
                                        | 4 -> FiveKind str
                                        | 5 -> FiveKind str
                                        | _ -> failwith ("Unhandled Hand " ^ str))
                        | 1 -> (match unpaired with
                                        | 0 -> FiveKind str
                                        | 1 -> FourKind str
                                        | 2 -> ThreeKind str
                                        | 3 -> OnePair str
                                        | _ -> failwith ("Unhandled Hand " ^ str))
                        | 2 -> if unpaired = 0 then Full str else TwoPair str
                        | _ -> failwith ("Unhandled Hand " ^ str)
        in
        let rec find_hand_type i in_a_run reps rems =
                if i >= (Array.length hand_arr) 
                        (* Need to terminate either the run or non-run before deducing hand *)
                        then (if in_a_run then deduce_hand (reps+1) rems else deduce_hand reps (rems+1))
                        else 
                                let is_pair = hand_arr.(i-1) = hand_arr.(i) in
                                (match (is_pair, in_a_run) with
                                        | false, false -> find_hand_type (i+1) false reps (rems+1)
                                        | false, true -> find_hand_type (i+1) false (reps+1) rems
                                        | true, _ -> find_hand_type (i+1) true reps rems)
        in
        find_hand_type 1 false 0 0

let read_game_state channel string_tf=
        let rec read_inner channel l =
                match Scanf.bscanf_opt channel " %s %d " (fun s d -> (make_hand (string_tf s), d)) with
                        | None -> l
                        | Some r -> read_inner channel (r :: l)
        in
        read_inner channel []

let run1 () =
        let game_state = List.stable_sort (fun (lhs, _) (rhs, _) -> compare_hand lhs rhs) 
                (read_game_state Scanf.Scanning.stdin (fun s -> s)) in
        List.iter (fun x -> print_endline (hand_to_string (fst x))) game_state;
        let winnings = List.fold_left ( + ) 0
                (List.mapi (fun i (_, bid) -> (i + 1) * bid) game_state) in
        Printf.printf "Total winnings: %d\n" winnings

let run2 () =
        let joker_tf = (fun s -> String.init (String.length s) (fun i -> if s.[i] = 'J' then 'X' else s.[i])) in
        let game_state = List.stable_sort (fun (lhs, _) (rhs, _) -> compare_hand lhs rhs) 
                (read_game_state Scanf.Scanning.stdin joker_tf) in
        List.iter (fun x -> print_endline (hand_to_string (fst x))) game_state;
        let winnings = List.fold_left ( + ) 0
                (List.mapi (fun i (_, bid) -> (i + 1) * bid) game_state) in
        Printf.printf "Total winnings: %d\n" winnings

