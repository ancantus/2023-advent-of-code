(* Utility functions *)
let is_digit = function
        | '0'..'9' -> true
        | _ -> false

let convert_digit = function
        | '0' -> Some 0
        | '1' -> Some 1
        | '2' -> Some 2
        | '3' -> Some 3
        | '4' -> Some 4
        | '5' -> Some 5
        | '6' -> Some 6
        | '7' -> Some 7
        | '8' -> Some 8
        | '9' -> Some 9
        | _ -> None

let string_find f str =
        let rec inner f str i = 
                if f str.[i] 
                then str.[i]
                else inner f str (i + 1) in
        inner f str 0

let string_rfind f str =
        let rec inner f str i =
                if f str.[i]
                then str.[i]
                else inner f str (i - 1) in
        inner f str ((String.length str) - 1)

(* Parsing a single line *)
let find_first_digit str = convert_digit (string_find is_digit str)
let find_last_digit str = convert_digit (string_rfind is_digit str)
let find_cal_values str = (find_first_digit str, find_last_digit str)
let calc_cal_value (a, b) = (Option.get a * 10) + Option.get b

(* Handling the diabolical case where "two" -> "2" *)
let starts_with ~prefix:search str = 
        let rec inner search len1 str len2 i =
                if i >= len1 
                        then true
                else if i >= len2
                        then false
                else if search.[i] <> Bigstring.get str i
                        then false
                else inner search len1 str len2 (i+1)
        in
        inner search (String.length search) str (Bigstring.length str) 0

let remap_str input output str = if starts_with ~prefix:input str then Some output else None
let cal_file_remap = [
        ("one", '1');
        ("two", '2');
        ("three", '3');
        ("four", '4');
        ("five", '5');
        ("six", '6');
        ("seven", '7');
        ("eight", '8');
        ("nine", '9');
]
let get_remapper map_table str =
        let remappers = List.map (fun (x, y) -> remap_str x y) map_table in
        let str_cache = Bigstring.of_string str in
        let str_len = Bigstring.length str_cache in
        (fun i -> List.fold_left 
                (fun acc f -> Option.value (f (Bigstring.sub str_cache i (str_len - i))) ~default:acc)
                (Bigstring.get str_cache i)
                remappers)
let remap_cal_line str =
        let cal_file_remapper = get_remapper cal_file_remap str in
        String.init (String.length str) cal_file_remapper

(* Parsing logic for multi-line *)
let accum_calc_values acc str =
        acc + (calc_cal_value (find_cal_values (remap_cal_line str)))
let run () = Printf.printf "Calibration Value: %i\n" (In_channel.fold_lines accum_calc_values 0 stdin)

