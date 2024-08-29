type spring_group =
        | Operational of int
        | Damaged of int
        | Unknown of int

type condition_record = {
        statement : spring_group list;
        summary : int list; (* summary of damaged groups *)
}

let print_condition_record r =
        let rec print_statement s =
                match s with 
                        | Operational i :: tail -> print_char '.'; 
                                print_statement ((if i > 1 then [Operational (i-1)] else []) @ tail)
                        | Damaged i :: tail -> print_char '#'; 
                                print_statement ((if i > 1 then [Damaged (i-1)] else []) @ tail)
                        | Unknown i :: tail -> print_char '?'; 
                                print_statement ((if i > 1 then [Unknown (i-1)] else []) @ tail)
                        | [] -> print_char ' '
        in
        let print_summary summary = 
                let rec inner s = 
                        match s with 
                                | [] -> ()
                                | i :: [] -> print_int i;
                                | i :: tail -> print_int i; print_char ','; print_char ' '; inner tail
                in
                print_char '(';
                inner summary;
                print_char ')'
        in
        print_statement r.statement;
        print_summary r.summary;
        print_endline ""

let read_condition_records channel =
        let rec parse_statement src i statements = 
                let append_group new_g statements =
                        match new_g, statements with
                                | Operational i, ((Operational j) :: tail) -> (Operational (i + j)) :: tail
                                | Damaged i, ((Damaged j) :: tail) -> (Damaged (i + j)) :: tail
                                | Unknown i, ((Unknown j) :: tail) -> (Unknown (i + j)) :: tail
                                | _, _ -> (new_g :: statements)
                in
                match src.[i] with
                        | ' ' -> (statements, i+1)
                        | '.' -> parse_statement src (i+1) (append_group (Operational 1) statements)
                        | '#' -> parse_statement src (i+1) (append_group (Damaged 1) statements)
                        | '?' -> parse_statement src (i+1) (append_group (Unknown 1) statements)
                        | c -> failwith ("Invalid condition record " ^ (String.make 1 c))
        in 
        let parse_summary src =
                List.rev (List.map int_of_string (String.split_on_char ',' src)) in
        let parse_record s = 
                let sub_to_end src i = String.sub src i ((String.length src) - i) in
                let (statement, i) = parse_statement s 0 [] in
                let summary = parse_summary (sub_to_end s i) in
                {statement; summary}
        in
        let rec inner records = 
                match In_channel.input_line channel with
                        | None -> records
                        | Some s -> inner ((parse_record s) :: records)
        in
        inner []

let choose n k =
        (* this choose function is going to mess me up! *)
        let rec factorial n = 
                match n with
                        | 1 -> 1
                        | i -> i * (factorial (i-1))
        in
        (factorial n) / ((factorial (n - k)) * (factorial k))

(* Required number of slots of unknown fields to contain the summary *)
let slot_count summary =
        (List.fold_left (fun acc i -> acc + (i + 1)) 0 summary) - 1

let is_summary_possible statement summary =
        let max_run = List.fold_left max 0 summary in
        let required_total = slot_count summary in
        let rec inner s count = 
                match s with
                        | [] -> (count >= required_total)
                        | Broken n :: tail -> if n > max_run then false else (inner tail (count + n)) 
                        | Unknown n :: tail -> (inner tail (count + n))
        inner statement 0

let partition in_statement in_summary out_statement out_summary broken_count =
        let sum_broken -> List.left_fold ( + ) in
        match in_statement with
                | Operational _ :: tail -> {statement=out_statement, summary=out_summary}
                | head :: tail -> if (is_summary_possible (head :: out_statement) out_summary)
        

let run () =
        let records = read_condition_records stdin in
        List.iter print_condition_record records


