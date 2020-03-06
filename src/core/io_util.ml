let read_all channel =
  let buffer = Buffer.create 1024 in
  let bytes = Bytes.create 8192 in
  let rec read () =
    let n = input channel bytes 0 8192 in
    Buffer.add_subbytes buffer bytes 0 n;
    if n > 0 then read ()
  in
  let () = read () in
  close_in channel; Buffer.contents buffer

let flush () =
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  flush stdout;
  flush stderr

let with_redirect file fn =
  let descr_prev_stdout = Unix.dup Unix.stdout in
  let descr_prev_stderr = Unix.dup Unix.stderr in
  (* Redirect stdout/stderr to our temporary file. *)
  flush ();
  let descr_file = Unix.(openfile file [ O_WRONLY; O_TRUNC; O_CREAT ] 0o600) in
  Unix.dup2 descr_file Unix.stdout;
  Unix.dup2 descr_file Unix.stderr;
  Unix.close descr_file;
  let r =
    match fn () with
    | x -> Ok x
    | exception e -> Error (e, Printexc.get_raw_backtrace ())
  in
  (* And redirect back to the original state. *)
  flush ();
  Unix.dup2 descr_prev_stdout Unix.stdout;
  Unix.dup2 descr_prev_stderr Unix.stderr;
  (* Close our original handles. *)
  Unix.close descr_prev_stdout;
  Unix.close descr_prev_stderr;
  match r with
  | Ok r ->
      let channel = open_in file in
      let contents = read_all channel in
      close_in channel; (r, contents)
  | Error (e, bt) -> Printexc.raise_with_backtrace e bt
