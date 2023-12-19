let sensor_readings channel =
        Seq.unfold (fun io -> match In_channel.input_line io with 
                | None -> None
                | Some s -> Some (List.rev (List.map int_of_string (String.split_on_char ' ' s)), io )) channel
                
let rec generate_history sensor_obs =
        let rec diff_line sensor_obs result = 
                match sensor_obs with
                        | [] -> List.rev result
                        | _ :: [] -> List.rev result
                        | a :: b :: tail -> diff_line (b :: tail) ((a - b) :: result)
        in
        match sensor_obs with
                | [] -> failwith "Bad Input"
                | latest :: old_history -> 
                        match List.find_opt (fun i -> i <> 0) latest with
                                | None -> sensor_obs
                                | Some _ -> generate_history ((diff_line latest []) :: latest :: old_history)

let predict_next sensor_obs f =
        let calc_predict line last_predict = 
                match line with
                        | [] -> failwith "Bad Input"
                        | a :: _ -> f a last_predict
        in
        let rec predict_next obs last_predict =
                match obs with
                        | [] -> failwith "Bad Input"
                        | line :: [] -> calc_predict line last_predict
                        | line :: tail -> predict_next tail (calc_predict line last_predict)
        in
        (* strip off the 0's list *)
        let predict_input = match sensor_obs with [] -> failwith "Bad Input" | 
                _ :: obs -> obs in
        predict_next predict_input 0

let predict_prev sensor_obs = 
        predict_next (List.map List.rev sensor_obs) ( - )

let run1 () =
        let sensor_seq = sensor_readings In_channel.stdin in
        let generated_history = Seq.map (fun obs -> generate_history [obs]) sensor_seq in
        let extrapolated_sum = Seq.fold_left (fun acc obs -> acc + (predict_next obs ( + ))) 0 generated_history in
        Printf.printf "Sum of extrapolated future values: %d\n" extrapolated_sum  

let run2 () =
        let sensor_seq = sensor_readings In_channel.stdin in
        let generated_history = Seq.map (fun obs -> generate_history [obs]) sensor_seq in
        let extrapolated_sum = Seq.fold_left (fun acc obs -> acc + (predict_prev obs)) 0 generated_history in
        Printf.printf "Sum of extrapolated past values: %d\n" extrapolated_sum  


