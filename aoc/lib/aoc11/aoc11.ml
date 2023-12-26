type space_objects =
        | Empty
        | Expanded
        | Galaxy of int

let load_img channel =
        let make_img str =
                let len = String.length str in
                Bigarray.Array2.init Bigarray.char Bigarray.c_layout len len (fun _ _ -> '.') in
        let first_line = Option.get (In_channel.input_line channel) in
        let img = make_img first_line in
        let rec load_impl i end_i str =
                if i >= end_i then
                        ()
                else
                        let _ = String.iteri (fun j c -> Bigarray.Array2.set img i j c) str in
                        load_impl (i+1) end_i (Option.value (In_channel.input_line channel) ~default:"")
                in
        let _ = load_impl 0 (Bigarray.Array2.dim2 img) first_line in
        img

let print_img img =
        let dim1 = Bigarray.Array2.dim1 img in
        let dim2 = Bigarray.Array2.dim2 img in
        let rec print_row row j =
                if j >= dim2 then Printf.printf "\n"
                else (Printf.printf "%c" row.{j}; print_row row (j+1))
        in
        let rec inner_print img i =
                if i >= dim1 then ()
                else (print_row (Bigarray.Array2.slice_left img i) 0; inner_print img (i+1))
        in
        inner_print img 0

let process_expansion img =
        let dim1 = Bigarray.Array2.dim1 img in
        let dim2 = Bigarray.Array2.dim2 img in
        let rec is_expansion acc coords =
                if fst coords >= dim1 || snd coords >= dim2 then true
                else (
                        if img.{fst coords, snd coords} = '#' then false
                        else is_expansion acc ((fst coords) + (fst acc), (snd coords) + (snd acc))
                )
        in
        let is_row i = is_expansion (0, 1) (i, 0) in
        let is_column j = is_expansion (1, 0) (0, j) in
        let set_row i =
                Bigarray.Array1.fill (Bigarray.Array2.slice_left img i) 'X' in
        let set_column j =
                let rec inner i = 
                        if i >= dim1 then ()
                        else (img.{i, j} <- 'X'; inner (i+1))
                in
                inner 0
        in
        for i = 0 to dim1 - 1 do
                if is_row i then set_row i else ()
        done;
        for j = 0 to dim2 - 1 do
                if is_column j then set_column j else ()
        done

let find_galaxies img =
        let dim1 = Bigarray.Array2.dim1 img in
        let dim2 = Bigarray.Array2.dim2 img in
        let rec inner i j galaxies =
                if i >= dim1 then galaxies
                else (
                        let next_j = (j + 1) mod dim2 in
                        let next_i = if next_j < j then i + 1 else i in
                        inner next_i next_j (if img.{i,j} = '#' then ((i, j) :: galaxies) else galaxies)
                )
        in
        inner 0 0 []

let rec make_pairs elements =
        match elements with
                | [] -> []
                | _ :: [] -> []
                | front :: tail -> (List.map (fun i -> (front, i)) tail) @ (make_pairs tail)

let get_distance img start dest =
        let get_next_step coords =
                let magnitude i = if i = 0 then 0 else i / (abs i) in
                let step_x = magnitude ((fst dest) - (fst coords)) in
                let step_y = magnitude ((snd dest) - (snd coords)) in
                if step_x <> 0 then ((fst coords) + step_x, snd coords)
                else (fst coords, (snd coords) + step_y)
        in
        let rec inner coords dist =
                if coords = dest then dist
                else (
                        let dist = if img.{fst coords, snd coords} = 'X' then dist + 2 else dist + 1 in
                        inner (get_next_step coords) dist               
                )
        in
        inner start 0

let run () =
        let img = load_img stdin in
        process_expansion img;
        print_img img;
        let galaxy_pairs = make_pairs (find_galaxies img) in
        let min_distance = List.fold_left (fun acc (g1, g2) -> acc + (get_distance img g1 g2)) 0 galaxy_pairs in
        Printf.printf "Minimum distance: %d\n" min_distance

