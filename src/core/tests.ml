open Core

type error =
  { message : Format.formatter -> unit;
    backtrace : Printexc.raw_backtrace option
    }

type result =
  | Pass
  | Skipped
  | Failed of error
  | Errored of error

let result_of_exn e =
  let message = Printexc.to_string e in
  Errored
    { message = (fun x -> Format.pp_print_text x message);
      backtrace = Some (Printexc.get_raw_backtrace ())
    }

type 'a tree =
  | TestCase of string * 'a
  | TestGroup of string * 'a tree list

module type Test = sig
  include Configurable

  val run : options -> result Lwt.t
end

type test = (module Test)

let test_case name test = TestCase (name, test)

let group name tests = TestGroup (name, tests)

let basic_options = Cmdliner.Term.const ()

let test_async name fn =
  test_case name
    ( module struct
      type options = unit

      let options = basic_options

      let run = fn
    end
    : Test )

let test name fn = test_async name (fun () -> Lwt.return (fn ()))

let simple_test name fn = test_async name (fun () -> fn (); Lwt.return Pass)

let pending name _ = test_async name (fun () -> Lwt.return Skipped)
