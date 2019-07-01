(** Information about an error produced by a test. *)
type error =
  { backtrace : Printexc.raw_backtrace option
        (** An optional backtrace to where this error occurred. *)
  }

(** The outcome of executing this test. Namely, whether it is successful or not. *)
type outcome =
  | Pass  (** This test completed without issue. *)
  | Skipped  (** This test was not executed. *)
  | Failed of error  (** One or more assertions in this test failed. *)
  | Errored of error  (** An unexpected error occurred. *)

(** The result of this test. Includes the {!outcome}, and additional information such as duration. *)
type result =
  { outcome : outcome;  (** This test's outcome. *)
    message : (Format.formatter -> unit) option;
        (** A message about this test, detailing information about the test execution or its reason
            for failure.

            As this is a function (rather than a string), and accepts a {!Format.formatter}, one
            can safely use colours and other formatting features without worrying how they'll be
            presented to the user. *)
    time : Mtime.span
        (** The time taken to execute this test. This can be left at {!Mtime.Span.zero} and the
            test runner will fill it in. *)
  }

(** The current state of test execution. *)
type status =
  | NotStarted  (** This test has not started execution yet. *)
  | Running  (** This test is currently being run. *)
  | Finished of result  (** This test has finished, and produced a test {!result}. *)

(** Construct a result from an outcome. *)
val result : ?message:(Format.formatter -> unit) option -> outcome -> result

(** Create a new {!Errored} {!result} from an exception and a starting time. *)
val result_of_exn : exn -> result

(** A tree of tests to run.

    Typically, these will be parameterised over {!Test}, but this will change when executing tests.

    You should not create {!tree}s directly - instead prefer the {!test} and {!group} factories.*)
type 'a tree =
  | TestCase of string * 'a
  | TestGroup of string * 'a tree list

(** An instance of a test.

    Generally you do not need to instantiate an instance of a {!Test} directly - one can normally
    get away with a helper method (such as {!test}). However, if you need to receive command line
    arguments, or require additional processing, one can derive directly from {!Test} *)
module type Test = sig
  include Core.Configurable

  (** Run this test with the given options. *)
  val run : options -> result Lwt.t
end

(** An alias over {!Test}, so you don't have to continuously use the module keyword. *)
type test = (module Test)

(** Create a test tree from a name and single test case. *)
val test_case : string -> test -> test tree

(** Create a new test group from several child test trees. *)
val group : string -> test tree list -> test tree

(** Create a simple test case from an asynchronous function.

    This test accepts no options (and so cannot be configured) and returns the result of executing
    it. *)
val test_async : string -> (unit -> result Lwt.t) -> test tree

(** Create a simple test case from a function.

    This test accepts no options (and so cannot be configured) and returns the result of executing
    it. *)
val test : string -> (unit -> result) -> test tree

(** Create a new test case from an action.

    This will pass if it completes, or error if it throws an exception. *)
val simple_test : string -> (unit -> unit) -> test tree

(** Create a "pending" test.

    This is one which has not yet been written, or whose functionality is currently broken. It will
    finish immediately and return {!Skipped}. *)
val pending : string -> 'a -> test tree
