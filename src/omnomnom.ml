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
        let promise, resolve = task () in
        ( TestCase (name, promise),
          (promise >|= fun x -> TestCase (name, x)),
          Cmdliner.Term.(
            const (fun args xs -> (resolve, fun () -> Test.run args) :: xs) $ Test.options $ tasks)
        )
    | TestGroup (name, children) ->
        let children, children_p, tasks =
          List.fold_left
            (fun (children, children_p, tasks) child ->
              let child, child_p, tasks = build_tree tasks child in
              (child :: children, child_p :: children_p, tasks) )
            ([], [], tasks) children
        in
        let rec collect acc = function
          | [] -> Lwt.return (TestGroup (name, acc))
          | x :: xs -> x >>= fun x -> collect (x :: acc) xs
        in
        (TestGroup (name, List.rev children), collect [] children_p, tasks)
  in
  let run tests result tasks options =
    match Reporter.run options with
    | None -> `Error (true, "No test reporter for these options.")
    | Some f ->
        let future = f tests result in
        tasks
        |> List.iter (fun (resolve, action) ->
               try on_any (action ()) (wakeup resolve) (fun e -> wakeup resolve (result_of_exn e))
               with e -> wakeup resolve (result_of_exn e) );
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
  let tests, results, tasks = build_tree (const []) tests in
  let results = results >>= fun r -> Lwt.pause () >|= fun () -> r in
  exit @@ eval (const (run tests results) $ tasks $ Reporter.options, info)
