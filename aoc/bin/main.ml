
let () = match Sys.argv.(1) with
        "aoc01" -> Aoc01.run ()
        | "aoc02.1" -> Aoc02.run1 ()
        | "aoc02.2" -> Aoc02.run2 ()
        | "aoc03" -> Aoc03.run ()
        | "aoc04" -> Aoc04.run ()
        | "aoc05.1" -> Aoc05.run1 ()
        | "aoc05.2" -> Aoc05.run2 ()
        | "aoc06.1" -> Aoc06.run1 ()
        | "aoc06.2" -> Aoc06.run2 ()
        | "aoc07.1" -> Aoc07.run1 ()
        | "aoc07.2" -> Aoc07.run2 ()
        | _ -> print_endline "Unhandled AOC"

