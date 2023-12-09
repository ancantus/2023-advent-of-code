
let () = match Sys.argv.(1) with
        "aoc01" -> Aoc01.run ()
        | _ -> print_endline "Unhandled AOC"

