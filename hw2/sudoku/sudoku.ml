let exit_with_msg msg =
  print_endline msg;
  exit 1

let () =
  let in_chan = Scanf.Scanning.stdin in
  let p = try
      Puzzle.from_channel in_chan
    with Puzzle.ParseError msg -> exit_with_msg (Printf.sprintf "parse error: %s" msg)
  in
  (* print_string (Puzzle.show p); *)
  let z3 = Z3.init () in
  (* Z3.raw_send z3 "(echo \"hello from z3\")"; *)
  let z3_push cmd = Z3.raw_send z3 cmd in
  let z3_read ()  = Z3.raw_read_line z3 in
  (* let assert_neq i j = z3_push "(assert (not (= " (get_variable_name i k) *)
  z3_push "(push)";
  (* print_endline (z3_read ()); *)
  Puzzle.emit_smt z3_push z3_read p;
  z3_push "(pop)";
  Z3.close z3
