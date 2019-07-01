include OmnomnomCore
open Tests

module Tests = struct
  include Tests
  module Golden = OmnomnomTests.Golden
end

module Ingredients = struct
  include Ingredients
  module ConsoleReporter = OmnomnomIngredients.Console_reporter
end

let run ?(reporter = (module Ingredients.ConsoleReporter : Ingredients.Reporter))
    (tests : test tree) : unit =
  let module Reporter = (val reporter) in
  let open Lwt in
  let rec build_tree tasks = function
    | TestCase (name, test) ->
        let module Test = (val test : Test) in
        let source, sink = Signal.create NotStarted in
        ( TestCase (name, sink),
          Cmdliner.Term.(
            const (fun args xs -> (source, fun () -> Test.run args) :: xs)
            $ Test.options $ tasks) )
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
    | TestCase (name, sink) ->
       (match Signal.get sink with
       | Finished r -> TestCase (name, r)
       | _ -> failwith (Printf.sprintf "Test %S hasn't finished" name))
    | TestGroup (name, children) -> TestGroup(name, List.map finish_tree children)
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
          if !pending = 0 then
            Lwt.wakeup_later resolve (finish_tree tests)
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
               try
                 on_any (action ()) finish (fun e -> finish (result_of_exn e))
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
