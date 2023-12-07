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

(* Parsing logic for multi-line *)
let accum_calc_values acc str =
        acc + (calc_cal_value (find_cal_values str))
let () = Printf.printf "Calibration Value: %i\n" (In_channel.fold_lines accum_calc_values 0 stdin)

