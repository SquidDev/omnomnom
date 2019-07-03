(** An OCaml test framework inspired by Tasty.

    {2 Usage} 
    {[
open Omnomnom.Tests

let () = Omnomnom.run @@ group "omnomnom"
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
      console in a colourful manner. *)
  module ConsoleReporter : Reporter = Console_reporter
end

(** Run a series of tests, with the provided reporter. *)
let run ?(reporter = (module Console_reporter : Ingredients.Reporter)) (tests : test tree) : unit =
  let module Reporter = (val reporter) in
  let open Lwt in
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
  let rec finish_tree = function
    | TestCase (name, sink) -> (
      match Signal.get sink with
      | Finished r -> TestCase (name, r)
      | _ -> failwith (Printf.sprintf "Test %S hasn't finished" name) )
    | TestGroup (name, children) -> TestGroup (name, List.map finish_tree children)
  in
  let run tests tasks options =
    match Reporter.run options with
    | None -> `Error (true, "No test reporter for these options.")
    | Some f ->
        let result, resolve = Lwt.task () in
        let future = f tests result in
        let pending = ref (List.length tasks) in
        let finish () =
          pending := !pending - 1;
          if !pending = 0 then Lwt.wakeup_later resolve (finish_tree tests)
        in
        tasks
        |> List.iter (fun (source, action) ->
               Signal.update source Running;
               let counter = Mtime_clock.counter () in
               let finish result =
                 let duration = Mtime_clock.count counter in
                 let result = { result with time = duration } in
                 Signal.update source (Finished result);
                 Signal.plug source;
                 finish ()
               in
               try on_any (action ()) finish (fun e -> finish (result_of_exn e))
               with e -> finish (result_of_exn e));
        if Lwt_main.run future then `Ok () else `Error (false, "")
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
  (* let results = results >>= fun r -> Lwt.pause () >|= fun () -> r in *)
  exit @@ eval (const (run tests) $ tasks $ Reporter.options, info)
