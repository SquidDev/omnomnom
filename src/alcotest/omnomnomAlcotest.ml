open Omnomnom.Tests
module A = Alcotest

type opt = { quick : bool }

let options =
  let open Cmdliner in
  let open Cmdliner.Arg in
  let env = Arg.env_var "ALCOTEST_QUICK_TESTS" in
  let quick =
    value & flag & info ~docs:"Alcotest" ~doc:"Run only the quick tests." ~env [ "q"; "quicktests" ]
  in
  Term.(const (fun quick -> { quick }) $ quick)

let of_alcotest_case ((name, speed, run) : unit A.test_case) =
  test_case name
    ( module struct
      type options = opt

      let options = options

      let run { quick } =
        if quick && speed = `Slow then result Skipped
        else
          try run (); result Pass
          with e ->
            let backtrace = Some (Printexc.get_raw_backtrace ()) in
            let message = Printexc.to_string e in
            let message = Some (fun x -> Format.pp_print_text x message) in
            (* Some horrible reflection code, somewhat lifted from Printexc in order to extract
               Check_error exceptions. *)
            let x = Obj.repr e in
            let constructor : string =
              if Obj.tag x <> 0 then Obj.magic (Obj.field x 0)
              else Obj.magic (Obj.field (Obj.field x 0) 0)
            in
            if constructor = "Alcotest.Check_error" then
              let f = Obj.field x 1 in
              let message =
                if Obj.is_block f && Obj.tag f = Obj.string_tag then
                  let message : string = Obj.magic f in
                  Some (fun x -> Format.pp_print_text x message)
                else message
              in
              result ~message (Failed { backtrace })
            else result ~message (Errored { backtrace })
    end : Test )

let of_alcotest ((name, test) : unit A.test) = group name (List.map of_alcotest_case test)

let of_alcotests name tests = group name (List.map of_alcotest tests)
