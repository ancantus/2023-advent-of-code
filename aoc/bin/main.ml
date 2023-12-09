
let () = match Sys.argv.(1) with
        "aoc01" -> Aoc01.run ()
        | "aoc02" -> Aoc02.run ()
        | _ -> print_endline "Unhandled AOC"

