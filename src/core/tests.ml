open Core

type error = { backtrace : Printexc.raw_backtrace option }

type outcome =
  | Pass
  | Skipped
  | Failed of error
  | Errored of error

type result =
  { outcome : outcome;
    message : (Format.formatter -> unit) option;
    time : Mtime.span
  }

type status =
  | NotStarted
  | Running
  | Finished of result

let result ?(message = None) outcome = { message; outcome; time = Mtime.Span.zero }

let result_of_exn e =
  let message = Printexc.to_string e in
  result
    ~message:(Some (fun x -> Format.pp_print_text x message))
    (Errored { backtrace = Some (Printexc.get_raw_backtrace ()) })

type 'a tree =
  | TestCase of string * 'a
  | TestGroup of string * 'a tree list

module type Test = sig
  include Configurable

  val run : options -> result
end

type test = (module Test)

let test_case name test = TestCase (name, test)

let group name tests = TestGroup (name, tests)

let basic_options = Cmdliner.Term.const ()

let test name fn =
  test_case name
    ( module struct
      type options = unit

      let options = basic_options

      let run = fn
    end : Test )

let simple_test name fn = test name (fun () -> fn (); result Pass)

let pending name _ = test name (fun () -> result Skipped)
