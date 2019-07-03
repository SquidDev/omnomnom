open Omnomnom.Tests

type options = { path : string option }

let writable_file =
  let parse s =
    if Sys.file_exists s && Sys.is_directory s then
      Error (`Msg (Printf.sprintf "%S is a directory" s))
    else
      let p = Filename.dirname s in
      if Sys.file_exists p && not (Sys.is_directory p) then
        Error (`Msg (Printf.sprintf "Cannot write to %S." s))
      else Ok s
  in
  Cmdliner.Arg.conv ~docv:"FILE" (parse, Format.pp_print_string)

let options =
  let open Cmdliner in
  let open Cmdliner.Arg in
  let path =
    value
    & opt ~vopt:(Some "report.xml") (some writable_file) None
    & info ~docs:"JUnit reporter" ~doc:"The JUnit XML file to write to." [ "junit" ]
  in
  Term.(const (fun path -> { path }) $ path)

let get_message message { backtrace } =
  let buffer = Buffer.create 16 in
  ( match message with
  | None -> ()
  | Some msg ->
      let formatter = Format.formatter_of_buffer buffer in
      msg formatter;
      Format.pp_force_newline formatter ();
      Format.pp_print_flush formatter () );
  ( match backtrace with
  | None -> ()
  | Some bt ->
      Buffer.add_string buffer (Printexc.raw_backtrace_to_string bt);
      Buffer.add_char buffer '\n' );
  Buffer.contents buffer

let rec get_result classname = function
  | TestCase (name, { outcome; message; time }) ->
      let open Junit.Testcase in
      let time = Mtime.Span.to_s time in
      let case =
        match outcome with
        | Pass -> pass ~name ~classname ~time
        | Skipped -> skipped ~name ~classname ~time
        | Failed e -> failure ~typ:"" ~name ~classname ~time (get_message message e)
        | Errored e -> error ~typ:"" ~name ~classname ~time (get_message message e)
      in
      ([ case ], [])
  | TestGroup (name, children) ->
      let open Junit.Testsuite in
      let name =
        match classname with
        | "" -> name
        | classname -> classname ^ "." ^ name
      in
      let cases, suites = get_results name children in
      let suites =
        match cases with
        | [] -> suites
        | cases -> (make ~name () |> add_testcases cases) :: suites
      in
      ([], suites)

and get_results name xs =
  List.fold_right
    (fun test (cases, suites) ->
      let cases', suites' = get_result name test in
      (cases' @ cases, suites' @ suites))
    xs ([], [])

let run_with path _ results =
  let%lwt tests = results in
  let suites =
    match get_result "" tests with
    | [], suites -> suites
    | cases, suites ->
        let open Junit.Testsuite in
        (make ~name:"Root" () |> add_testcases cases) :: suites
  in
  Junit.to_file (Junit.make suites) path;
  Lwt.return true

let run = function
  | { path = None } -> None
  | { path = Some path } -> Some (run_with path)
