type t = unit (* delete "unit" and put your datastructure here *)

exception ParseError of string

let from_channel s =
  let get_int () =
    try
      Scanf.bscanf s " %d" (fun n -> n)
    with
      Scanf.Scan_failure msg ->
      raise (ParseError (Printf.sprintf "failed to read integer: %s" msg))
    | End_of_file ->
      raise (ParseError "Unexpected end of file")
  in
  let k = get_int () in
  if k <= 0 then raise (ParseError (Printf.sprintf "k must be > 0, but got %d" k));
  if k > 100 then raise (ParseError (Printf.sprintf "k must be <= 100, got %d" k));
  (* now call get_int k**4 times and store the numbers in the puzzle *)
  failwith "unimplemented"

let show _ = failwith "unimplemented"

(* Uncomment this test when you have implemented from_channel and show. *)
(* 
let%test_module _ =
  (module struct
     let p = from_channel (Scanf.Scanning.from_string
                             "2\n1 2 3 4\n3 4 1 2\n2 1 4 3\n4 3 2 1\n")
     let%expect_test _ =
       print_endline (show p);
       [%expect{|
         2
         1 2 3 4
         3 4 1 2
         2 1 4 3
         4 3 2 1
       |}]
   end)
 *)
