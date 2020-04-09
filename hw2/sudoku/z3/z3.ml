type t =
  { proc_pid : int
  ; proc_out : in_channel
  ; proc_in : out_channel
  ; proc_err : in_channel
  }

let raw_send z3 msg =
  (* uncomment this see what you are sending to z3 to help debug *)
  (* Printf.printf "sending '%s' to z3\n%!" msg; *)
  output_string z3.proc_in msg;
  output_string z3.proc_in "\n";
  flush z3.proc_in

let raw_read_line z3 =
  input_line z3.proc_out

let raw_read_err z3 =
  input_line z3.proc_err

(* The Unix library in OCaml is a little janky when opening processes. 
   I copy-pasted these functions from the source code and edited them.
 *)
module MyUnix = struct
  let open_proc prog args =
    let open Unix in
    let (in_read, in_write) = pipe ~cloexec:true () in
    let (out_read, out_write) =
      try pipe ~cloexec:true ()
      with e -> close in_read; close in_write; raise e in
    let (err_read, err_write) =
      try pipe ~cloexec:true ()
      with e -> close in_read; close in_write;
                close out_read; close out_write; raise e in
    let inchan = in_channel_of_descr in_read in
    let outchan = out_channel_of_descr out_write in
    let errchan = in_channel_of_descr err_read in
    let pid = 
      try
        create_process prog args out_read in_write err_write
      with e ->
        close out_read; close out_write;
        close in_read; close in_write;
        close err_read; close err_write;
        raise e
    in
    close out_read;
    close in_write;
    close err_write;
    (pid, inchan, outchan, errchan)  
  
  let rec waitpid_non_intr pid =
    let open Unix in 
    try waitpid [] pid
    with Unix_error (EINTR, _, _) -> waitpid_non_intr pid
  
  let close_proc (pid, inchan, outchan, errchan) =
    close_in inchan;
    begin try close_out outchan with Sys_error _ -> () end;
    close_in errchan;
    snd(waitpid_non_intr pid)
end
      
let init () =
  let (z3pid, z3out, z3in, z3err) =
    MyUnix.open_proc "z3" [|"z3"; "-in"|]
  in
  let z3 =
    { proc_pid = z3pid
    ; proc_out = z3out
    ; proc_in = z3in
    ; proc_err = z3err
    }
  in
  z3

let close_input z3 =
  close_out z3.proc_in

let close z3 =
  close_input z3;
  let rec read_loop prefix f () =
    try
      let line = f () in
      print_string prefix;
      print_endline line;
      read_loop prefix f ()
    with
      End_of_file -> ()
  in
  read_loop "extra data on z3 stdout: " (fun () -> raw_read_line z3) ();
  read_loop "extra data on z3 stderr: " (fun () -> raw_read_err z3) ();

  ignore (MyUnix.close_proc (z3.proc_pid, z3.proc_out, z3.proc_in, z3.proc_err))

