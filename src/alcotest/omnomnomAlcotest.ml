open Omnomnom.Tests
module A = Alcotest

type opt = { quick : bool }

let options =
  let open Cmdliner in
  let open Cmdliner.Arg in
  let env = Cmd.Env.info "ALCOTEST_QUICK_TESTS" in
  let quick =
    value & flag & info ~docs:"Alcotest" ~doc:"Run only the quick tests." ~env [ "q"; "quicktests" ]
  in
  Term.(const (fun quick -> { quick }) $ quick)

let of_alcotest_case ((name, speed, run) : unit A.test_case) =
  test_case name
    (module struct
      type options = opt

      let options = options

      let run { quick } =
        if quick && speed = `Slow then result Skipped
        else
          try run (); result Pass
          with e -> (
            let backtrace = Some (Printexc.get_raw_backtrace ()) in
            let msg m x = Format.pp_print_text x m in
            (* Some horrible reflection code in order to extract Check_error exceptions. *)
            match e with
            | Alcotest_engine__Core.Check_error m ->
                result ~message:(Fun.flip m ()) (Failed { backtrace })
            | Failure e ->
                result ~message:(Printf.sprintf "Failure: %s" e |> msg) (Errored { backtrace })
            | _ -> result ~message:(Printexc.to_string e |> msg) (Errored { backtrace }))
    end : Test)

let mk_alcotest_case name speed run = of_alcotest_case (A.test_case name speed run)
let of_alcotest ((name, test) : unit A.test) = group name (List.map of_alcotest_case test)
let of_alcotests name tests = group name (List.map of_alcotest tests)
