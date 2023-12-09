type game_record = 
        | Red of int
        | Green of int
        | Blue of int

let get_int r = match r with
        Red n -> n
        | Green n -> n
        | Blue n -> n
let get_red r ~default:d = List.fold_left (fun acc a -> match a with Red n -> n | _ -> acc) d r
let get_green r ~default:d = List.fold_left (fun acc a -> match a with Green n -> n | _ -> acc) d r
let get_blue r ~default:d = List.fold_left (fun acc a -> match a with Blue n -> n | _ -> acc) d r

type game_summary = {
        index: int;
        records: (game_record list) list;
}
exception BadGameRecord of string 
exception MissingQuery of string

let parse_game_record str = List.map 
        (fun s -> Scanf.sscanf s " %d %s" (fun n colour -> match colour with 
                "red" -> Red n | "green" -> Green n | "blue" -> Blue n 
                | _ -> raise (BadGameRecord colour)))
        (String.split_on_char ',' str) 

let parse_query str =
        if String.starts_with ~prefix:"?" str
                then parse_game_record (String.sub str 1 (String.length str - 1))
        else raise (MissingQuery "str")

let parse_game_summary str =
        let (index, i) = Scanf.sscanf str "Game %d: %n" (fun x n -> (x, n)) in
        {
                index = index;
                records = List.map parse_game_record
                        (String.split_on_char ';' (String.sub str i (String.length str - i)));
        }
let parse_game_summaries = List.map parse_game_summary

let left_fold_game_records f acc records =
        let acc_records = Array.make 3 0 in
        let _ = List.iter (fun i -> match i with 
                Red n -> acc_records.(0) <- n 
                | Green n -> acc_records.(1) <- n
                | Blue n -> acc_records.(2) <- n) acc in
        let fold_game_record record = match record with 
                Red n -> acc_records.(0) <- f acc_records.(0) n
                | Green n -> acc_records.(1) <- f acc_records.(1) n
                | Blue n -> acc_records.(2) <- f acc_records.(2) n in
        let _ = List.iter (fun r -> List.iter fold_game_record r) records in
        (Red acc_records.(0), Green acc_records.(1), Blue acc_records.(2))
let get_max_cubes = left_fold_game_records max []

let game_filter query records =
        let (r_max, g_max, b_max) = get_max_cubes records in
        List.fold_left
                (fun acc a -> match a with
                        Red n -> if (get_int r_max) > n then false else acc
                        | Green n -> if (get_int g_max) > n then false else acc
                        | Blue n -> if (get_int b_max) > n then false else acc)
                true query

let run () = 
        let game_query = parse_query (Option.value (In_channel.input_line stdin) ~default:"") in
        let game_summaries = parse_game_summaries (In_channel.input_lines stdin) in
        let filtered_games = List.filter (fun summary -> game_filter game_query summary.records) game_summaries in
        Printf.printf "Accumulated Game Ids: %i\n" (List.fold_left (fun acc i -> acc + i.index) 0 filtered_games) 
