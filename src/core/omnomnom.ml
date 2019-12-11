(** An OCaml test framework inspired by Tasty.

    {2 Usage}

    {[
      open Omnomnom.Tests

      let () =
        Omnomnom.run
        @@ group "omnomnom"
             [ simple_test "Will assert a value" (fun () ->
                   assert (List.hd [ 0 ] = 0);
                   ());
               simple_test "Will fail" (fun () ->
                   let _ = List.hd [] in
                   ());
               pending "A test which needs to be written." ()
             ]
    ]} *)

[@@@ocamlformat "wrap-comments=false"]

open Tests
include Core
module Formatting = Formatting
module Signal = Signal
module Tests = Tests

module Ingredients = struct
  include Ingredients

  (** The default reporter (as used by {!run}). This prints tests progress and results to the
      console in a colourful manner.

      See {!console_reporter} for a more convenient to use version. *)
  module ConsoleReporter : Reporter = Console_reporter

  (** The default reporter (as used by {!run}). This prints tests progress and results to the
      console in a colourful manner.

      This is a first-class module version of {!ConsoleReporter}.*)
  let console_reporter : reporter = (module Console_reporter)
end

(** Run a series of tests, with the provided reporter. *)
let run ?(reporter = Ingredients.console_reporter) (tests : test tree) : unit =
  let module Reporter = (val reporter) in
  let output_file = Filename.temp_file "omnomnom-" "-output" in
  let rec build_tree tasks = function
    | TestCase (name, test) ->
        let module Test = (val test : Test) in
        let source, sink = Signal.create NotStarted in
        ( TestCase (name, sink),
          Cmdliner.Term.(
            const (fun args xs -> (source, fun () -> Test.run args) :: xs) $ Test.options $ tasks)
        )
    | TestGroup (name, children) ->
        let children, tasks =
          List.fold_left
            (fun (children, tasks) child ->
              let child, tasks = build_tree tasks child in
              (child :: children, tasks))
            ([], tasks) children
        in
        (TestGroup (name, List.rev children), tasks)
  in
  let is_success = function
    | Pass | Skipped -> true
    | Failed _ | Errored _ -> false
  in
  let rec finish_tree = function
    | TestCase (name, sink) -> (
      match Signal.get sink with
      | Finished r -> (is_success r.outcome, TestCase (name, r))
      | _ -> failwith (Printf.sprintf "Test %S hasn't finished" name) )
    | TestGroup (name, children) ->
        let ok, children =
          List.fold_left
            (fun (ok, children) child ->
              let ok', child = finish_tree child in
              (ok && ok', child :: children))
            (true, []) children
        in
        (ok, TestGroup (name, List.rev children))
  in
  let run tests tasks options =
    match Reporter.run options with
    | None -> `Error (true, "No test reporter for these options.")
    | Some f ->
        let callback = f tests in
        tasks |> List.rev
        |> List.iter (fun (source, action) ->
               Signal.update source Running;
               let counter = Mtime_clock.counter () in
               let finish result =
                 let duration = Mtime_clock.count counter in
                 let result = { result with time = duration } in
                 Signal.update source (Finished result);
                 Signal.plug source
               in
               let res, out =
                 Io_util.with_redirect output_file (fun () ->
                     try action () with e -> result_of_exn e)
               in
               let res =
                 match out with
                 | "" -> res
                 | msg ->
                     { res with
                       message =
                         Some
                           (fun out ->
                             Format.pp_print_text out msg;
                             Option.iter (fun x -> x out) res.message)
                     }
               in
               finish res);
        let ok, children = finish_tree tests in
        callback children;
        if ok then `Ok () else `Error (false, "")
  in
  (* Small bits of setup. *)
  Printexc.record_backtrace true;
  Random.self_init ();
  (* And run! *)
  let open Cmdliner.Term in
  let info =
    let doc = "A fancy test runner based on Tasty." in
    info "omnomnom" ~doc ~exits:default_exits
  in
  let tests, tasks = build_tree (const []) tests in
  let res = eval ~catch:true (const (run tests) $ tasks $ Reporter.options, info) in
  Sys.remove output_file; exit res
