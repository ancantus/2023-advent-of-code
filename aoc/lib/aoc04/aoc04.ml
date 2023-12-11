type card = {
        index: int;
        winning: int list;
        possible: int list;
}

let rec read_cards channel cards =
        let read_index channel = Scanf.bscanf_opt channel "Card %d: " (fun i -> i) in
        let rec read_num_list channel l = 
                let num = Scanf.bscanf_opt channel " %d " (fun i -> i) in
                match num with None -> l | Some n -> read_num_list channel (n :: l)
        in
        let consume channel = 
                match Scanf.bscanf_opt channel (" | ") (fun () -> ()) with
                        | _ -> ()
        in
        let read_card channel index =
                let winning = List.sort_uniq compare (read_num_list channel []) in
                consume channel;
                let possible = List.sort compare (read_num_list channel []) in
                { index; winning; possible }
        in
        let card_index = read_index channel in
        match card_index with 
                | None -> cards
                | Some idx -> read_cards channel ((read_card channel idx) :: cards)

let count_matches card =
        let rec find_scored winners n =
                match winners with
                        | [] -> None
                        | w :: tail -> if w = n then Some winners 
                                else find_scored tail n
        in
        let rec inner winners possible cnt =
                match possible with
                        | [] -> cnt
                        | n :: tail -> (match (find_scored winners n) with 
                                | None -> inner winners tail cnt
                                | Some winners -> inner winners tail (cnt+1))
        in
        inner card.winning card.possible 0

let calc_score cnt =
        let rec inner cnt score =
                match cnt with
                        | -1 -> 0
                        | 0 -> score
                        | n -> inner (n - 1) (score * 2) 
        in
        inner (cnt - 1) 1
                                
let print_card card =
        let print_int_list l = List.iter (Printf.printf " %02d ") l in
        Printf.printf "Card: %03i: " card.index;
        print_int_list card.winning;
        Printf.printf " | ";
        print_int_list card.possible;
        Printf.printf "\n"

let run () =
        let cards = read_cards Scanf.Scanning.stdin [] in
        List.iter print_card cards;
        let sum_score = List.fold_left (fun acc c -> acc + (calc_score (count_matches c))) 0 cards in
        Printf.printf "Total Score: %d\n" sum_score


