(** Ingredients are the primary mechanism to customise how tests are executed. *)

(** An interface for modules subscribe to test progress updates, and produce an output in some way. *)
module type Reporter = sig
  include Core.Configurable

  (** Takes a set of options and returns a subscriber function if this reporter is currently active.

      The signature of the subscriber function is a little bit misleading. It is first applied with
      the tree of current test progress, represented as a tree of result sinks. This can be used to
      monitor (and thus report on) the current state of test execution, or can happily be discarded.

      When all tests have finished executing, we apply the returned callback with the tree of all
      test results. This may be used to display a final report on the tests. *)
  val run :
    options -> (Tests.status Signal.sink Tests.tree -> Tests.result Tests.tree -> unit) option
end

(** A convenient type alias for first-class {!Reporter} instances. *)
type reporter = (module Reporter)
