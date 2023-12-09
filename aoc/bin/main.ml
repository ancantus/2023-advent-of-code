
let () = match Sys.argv.(1) with
        "aoc01" -> Aoc01.run ()
        | "aoc02.1" -> Aoc02.run1 ()
        | "aoc02.2" -> Aoc02.run2 ()
        | _ -> print_endline "Unhandled AOC"

