open Tests
include Core
module Formatting = Formatting
module Signal = Signal
module Tests = Tests

module Ingredients = struct
  include Ingredients

  let console_reporter : reporter = (module Console_reporter)

  let pattern_filter : filter = (module Pattern_filter)

  let void_reporter : reporter =
    ( module struct
      include Core.NoConfiguration

      let run () = None
    end )

  let void_filter : filter =
    ( module struct
      include Core.NoConfiguration

      let filter () x = x

      let results () _ = ()
    end )

  module ComposeOptions (L : Configurable) (R : Configurable) = struct
    type options = L.options * R.options

    let options =
      let open Cmdliner.Term in
      const (fun a b -> (a, b)) $ L.options $ R.options
  end

  let compose_reporters left right : reporter =
    let module Left = (val left : Reporter) in
    let module Right = (val right : Reporter) in
    ( module struct
      include ComposeOptions (Left) (Right)

      let run (a, b) =
        match (Left.run a, Right.run b) with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some a, Some b ->
            Some
              (fun p ->
                let a = a p and b = b p in
                fun r -> a r; b r)
    end )

  let compose_filters left right : filter =
    let module Left = (val left : Filter) in
    let module Right = (val right : Filter) in
    ( module struct
      include ComposeOptions (Left) (Right)

      let filter (a, b) xs = Left.filter a xs |> Right.filter b

      let results (a, b) xs = Left.results a xs; Right.results b xs
    end )

  let fold ~default ~compose = function
    | [] -> default
    | x :: xs -> List.fold_left compose x xs
end

let rec map_term f = function
  | [] -> Cmdliner.Term.const []
  | x :: xs -> Cmdliner.Term.(const List.cons $ f x $ map_term f xs)

let rec add_options = function
  | TestCase (name, test) ->
      let module Test = (val test : Test) in
      let mk args = TestCase (name, fun () -> Test.run args) in
      Cmdliner.Term.(const mk $ Test.options)
  | TestGroup (name, tests) ->
      let mk tests = TestGroup (name, tests) in
      Cmdliner.Term.(const mk $ map_term add_options tests)

module Runner = struct
  let rec build_tree tasks = function
    | TestCase (name, action) ->
        let source, sink = Signal.create NotStarted in
        (TestCase (name, sink), (source, action) :: tasks)
    | TestGroup (name, children) ->
        let children, tasks =
          List.fold_left
            (fun (children, tasks) child ->
              let child, tasks = build_tree tasks child in
              (child :: children, tasks))
            ([], tasks) children
        in
        (TestGroup (name, List.rev children), tasks)

  let is_success = function
    | Pass | Skipped -> true
    | Failed _ | Errored _ -> false

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

  let main (type r f) (module Reporter : Ingredients.Reporter with type options = r)
      (module Filter : Ingredients.Filter with type options = f) output_file tests (reporter : r)
      (filter : f) =
    match Reporter.run reporter with
    | None ->
        Printf.eprintf "No test reporter for these options.\n";
        1
    | Some f ->
        let tests, tasks = Filter.filter filter tests |> build_tree [] in
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
        if ok then 0 else 1
end

module type S = sig
  val tests : Tests.tests
end

let run ?(reporters = [ Ingredients.console_reporter ]) ?(filters = [ Ingredients.pattern_filter ])
    (tests : tests) : unit =
  let module Reporter =
  (val Ingredients.(fold ~default:void_reporter ~compose:compose_reporters reporters))
  in
  let module Filter =
  (val Ingredients.(fold ~default:void_filter ~compose:compose_filters filters))
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
  let output_file = Filename.temp_file "omnomnom-" "-output" in
  let res =
    eval ~catch:true
      ( const (Runner.main (module Reporter) (module Filter) output_file)
        $ add_options tests $ Reporter.options $ Filter.options,
        info )
  in
  Sys.remove output_file; exit_status res
