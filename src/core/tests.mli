(** An error produced by a test. *)
type error =
  { message : Format.formatter -> unit;
        (** A short description about this error.

            As this is a function (rather than a string), and accepts a {!Format.formatter}, one
            can safely use colours and other formatting features without worrying how they'll be
            presented to the user. *)
    backtrace : Printexc.raw_backtrace option
        (** An optional backtrace to where this error occurred. *)
    }

(** The result of executing a test. *)
type result =
  | Pass  (** This test completed without issue. *)
  | Skipped  (** This test was not executed. *)
  | Failed of error  (** One or more assertions in this test failed. *)
  | Errored of error  (** An unexpected error occurred. *)

(** Create a new {!Errored} {!result} from an exception. *)
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
