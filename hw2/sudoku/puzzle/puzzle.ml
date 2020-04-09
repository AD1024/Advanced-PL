exception ParseError of string

module GridKey =
    struct
      type t = (int * int)
      let compare = Stdlib.compare
    end

module GridMap = Map.Make (GridKey)

type t = int * (int GridMap.t)

let product xa xb = 
  List.concat 
      (List.map 
          (fun e -> List.map (fun e' -> (e,e')) xb) 
      xa)

let get_coordniate i k = (i / (k * k), i mod (k * k))

let get_variable_name coordinate : string =
  match coordinate with
    | (x, y) -> "x" ^ (string_of_int x) ^ "y" ^ (string_of_int y)

let assert_neq c1 c2 =
  let (v1, v2) = (get_variable_name c1, get_variable_name c2) in
  Printf.sprintf "(assert (not (= %s %s)))" v1 v2

let assert_eqconst coordinate value =
  let id = get_variable_name coordinate in
  Printf.sprintf "(assert (= %s %d))" id value

let assert_neqconst coordinate value =
  let id = get_variable_name coordinate in
  Printf.sprintf "(assert (not (= %s %d)))" id value

let assert_bound coordinate lower_bound upper_bound = 
  let id = get_variable_name coordinate in
  Printf.sprintf "(assert (and (>= %s %d) (<= %s %d)))" id lower_bound id upper_bound

let assert_distinct variables =
  let cartisian = product variables variables in
  Printf.sprintf "(assert (and %s))" 
                      (List.fold_left (^) "" (List.map 
                                                (fun (car, cdr) -> 
                                                    if car != cdr 
                                                    then Printf.sprintf "(not (= %s %s))" car cdr
                                                    else "") cartisian))

let declare_val i k =
  let id = (get_variable_name (get_coordniate i k)) in
  Printf.sprintf "(declare-const %s Int)" id

let eval_id i k =
  let id = (get_variable_name (get_coordniate i k)) in
  Printf.sprintf "(eval %s)" id

let from_channel (s : Scanf.Scanning.in_channel) : t =
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
  let rec init_map (dep : int) (grid_map : int GridMap.t) : int GridMap.t =
    if dep == k * k * k * k then
      grid_map
    else
      let num = get_int () in
      let new_map = GridMap.add 
                    (get_coordniate dep k)
                    num  grid_map in
      init_map (dep + 1) new_map
  in (k, init_map 0 GridMap.empty)
  

let show (sudoku : t) =
  match sudoku with
    | (k, grid) -> let () = Printf.printf "%d\n" k in
                      let rec process i =
                            if i == k * k * k * k then ""
                            else let num = GridMap.find (i / (k * k), i mod (k * k)) grid in
                                 let tail = process (i + 1) in
                                 if i mod (k * k) == k * k - 1
                                 then string_of_int num ^ (if (i / (k * k) != k * k - 1) then "\n" else "") ^ tail
                                 else string_of_int num ^ " "  ^ tail
                      in
                        process 0

let grid_lookup coordinate grid =
  try
    let num = GridMap.find coordinate grid in Some num
    with _ -> None

let rec parse_model z3_send z3_readline grid k dep =
  if dep == k * k * k * k then (GridMap.empty, GridMap.empty)
  else
    let (new_grid, banned) = parse_model z3_send z3_readline grid k (dep + 1) in
    let coordinate = get_coordniate dep k in
    let () = z3_send (eval_id dep k) in
    let new_val = int_of_string (z3_readline ()) in
    let result_grid = GridMap.add coordinate new_val new_grid in
    let result_banned = if GridMap.find coordinate grid == 0
                        then GridMap.add coordinate new_val banned
                        else banned in
    (result_grid, result_banned)


let rec solve dep z3_send z3_readline (sudoku : t) (banned : int GridMap.t) = 
  let (k, grid) = sudoku in
  let () = z3_send "(push)" in
  (* Process a k * k cell *)
  let rec process_cell start_x start_y cx cy k =
      if (cx - start_x + 1 >= k) && (cy - start_y + 1 >= k) then []
      else 
        let (next_x, next_y) = if cy - start_y + 1 == k then (cx + 1, start_y) else (cx, cy + 1) in
        get_variable_name (cx, cy) :: process_cell start_x start_y next_x next_y k in
  (* Emit constraints to Z3 for all cells *)
  let rec distinct_cells x y k =
      if x < k * k - 1 && y < k * k
      then let () = z3_send (assert_distinct (process_cell x y x y k)) in
           (* let () = Printf.printf "k : %d Cell UL: (%d %d)\n" k x y; flush stdout in *)
           let (next_x, next_y) = if y + k >= k * k then (x + k, 0) else (x, y + k)
           in  distinct_cells next_x next_y k in
  (* Declare Variables *)
  let () = for i = 0 to k * k * k * k - 1 do
              z3_send (declare_val i k)
           done in
  (* Assert Bounds EQ / NEQ Constants *)
  (* let () = print_endline "Assert Consts" in *)
  let () = for i = 0 to k * k * k * k - 1 do
              let coordinate = get_coordniate i k in
              let () = z3_send (assert_bound coordinate 1 (k * k)) in
              (* let (x, y) = coordinate in *)
              (* let () = Printf.printf "(%d %d)\n" x y; flush stdout in *)
              match grid_lookup coordinate banned with
                | Some value -> z3_send (assert_neqconst coordinate value)
                | None -> let num = GridMap.find (get_coordniate i k) grid in
                          if num != 0
                          then z3_send (assert_eqconst coordinate num)
           done in
  (* Rows / Cols *)
  (* let () = print_endline "Distinct Rows / Cols" in *)
  let () = for x1 = 0 to k * k - 1 do
              for x2 = 0 to k * k - 1 do
                for y = 0 to k * k - 1 do
                  if x1 != x2 then
                    let () = z3_send (assert_neq (x1, y) (x2, y)) in
                    let () = z3_send (assert_neq (y, x1) (y, x2)) in ()
                done
              done
           done in
  (* Cells *)
  (* let () = print_endline "Distinct Cells" in *)
  let () = distinct_cells 0 0 k in
  (* let () = print_endline "Check sat" in *)
  let () = z3_send "(check-sat)" in
  let result = z3_readline () in
  (* let () = print_endline result in *)
  if compare result "unsat" == 0 then
    let () = if dep == 0 then print_endline "unsat" 
             else print_endline "puzzle solution is unique"
    in z3_send "(pop)"
  else
    let (new_grid, banned_value) = parse_model z3_send z3_readline grid k 0 in
    let () = if dep > 0 then print_endline "found more than one solution!" in
    let () = print_endline (show (k, new_grid)) in
    let () = z3_send "(pop)" in
    if dep == 0 then solve (dep + 1) z3_send z3_readline (k, grid) banned_value
    

let emit_smt f1 f2 sudoku = solve 0 f1 f2 sudoku GridMap.empty
  

(* Uncomment this test when you have implemented from_channel and show. *)

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

