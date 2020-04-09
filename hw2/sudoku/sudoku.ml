let exit_with_msg msg =
  print_endline msg;
  exit 1

let () =
  let in_chan = Scanf.Scanning.stdin in
  let p = try
      Puzzle.from_channel in_chan
    with Puzzle.ParseError msg -> exit_with_msg (Printf.sprintf "parse error: %s" msg)
  in
  print_string (Puzzle.show p);
  let z3 = Z3.init () in
  Z3.raw_send z3 "(echo \"hello from z3\")";
  print_endline (Z3.raw_read_line z3);
  Z3.close z3
