
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
        | "aoc08.1" -> Aoc08.run1 ()
        | "aoc08.2" -> Aoc08.run2 ()
        | "aoc09.1" -> Aoc09.run1 ()
        | "aoc09.2" -> Aoc09.run2 ()
        | "aoc10" -> Aoc10.run ()
        | "aoc11" -> Aoc11.run ()
        | "aoc12" -> Aoc12.run ()
        | _ -> print_endline "Unhandled AOC"

