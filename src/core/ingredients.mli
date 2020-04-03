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

(** Filters may modify the tests to be executed. Generally this should only filter or rearrange the
    tests to be executed. *)
module type Filter = sig
  include Core.Configurable

  (**Filter the current test tree. Note, while there is no requirement that the structure of the
     tree is preserved, we recommend it. *)
  val filter : options -> 'a Tests.tree -> 'a Tests.tree

  (** Called after all tests have executed. This may be used to perform some final processing of the
      results. It should {i not} be used to display or report test results, use a {!Reporter} for
      that. *)
  val results : options -> Tests.result Tests.tree -> unit
end

(** A convenient type alias for first-class {!Filter} instances. *)
type filter = (module Filter)
