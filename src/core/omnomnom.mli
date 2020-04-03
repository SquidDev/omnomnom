(** An OCaml test framework inspired by Tasty.

    {2 Usage}

    {[
      open Omnomnom.Tests

      let () =
        Omnomnom.run
        @@ group "omnomnom"
             [ simple_test "Will assert a value" (fun () ->
                   assert (List.hd [ 0 ] = 0);
                   ());
               simple_test "Will fail" (fun () ->
                   let _ = List.hd [] in
                   ());
               pending "A test which needs to be written." ()
             ]
    ]} *)

include module type of Core

module Formatting = Formatting
module Signal = Signal
module Tests = Tests

module Ingredients : sig
  include module type of Ingredients

  (** The default reporter (as used by {!run}). This prints tests progress and results to the
      console in a colourful manner.

      See {!console_reporter} for a more convenient to use version. *)
  module ConsoleReporter : Reporter

  (** The default reporter (as used by {!run}). This prints tests progress and results to the
      console in a colourful manner.

      This is a first-class module version of {!ConsoleReporter}.*)
  val console_reporter : reporter
end

(** Run a series of tests, with the provided reporter. *)
val run : ?reporters:Ingredients.reporter list -> Omnomnom__.Tests.tests -> unit
