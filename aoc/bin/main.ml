
let () = match Sys.argv.(1) with
        "aoc01" -> Aoc01.run ()
        | "aoc02.1" -> Aoc02.run1 ()
        | "aoc02.2" -> Aoc02.run2 ()
        | "aoc03" -> Aoc03.run ()
        | "aoc04" -> Aoc04.run ()
        | _ -> print_endline "Unhandled AOC"

