(** Ingredients are the primary mechanism to customise how tests are executed. *)

(** An interface for modules subscribe to test progress updates, and produce an output in some way. *)
module type Reporter = sig
  include Core.Configurable

  (** Takes a set of options and returns a subscriber function if this reporter is currently
      active.

      The subscriber function accepts the test tree in several forms:

      - A partial tree (a tree of result event sinks), useful for monitoring the current state of
      test execution.

      - A complete tree (a promise of a result tree), useful for getting the final state of
      executing. This can be derived from the partial tree, and so is offered as a convenience.

      This function should return when all tests have completed execution, with a boolean
      determining whether it considers all tests having passed. *)
  val run :
    options ->
    (Tests.status Signal.sink Tests.tree -> Tests.result Tests.tree Lwt.t -> bool Lwt.t) option
end

(** A convenient type alias for first-class {!Reporter} instances. *)
type reporter = (module Reporter)

(** Compose multiple test reporters together. *)
val compose_reporters : reporter -> reporter -> reporter
