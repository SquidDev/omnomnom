(** Allows running QCheck tests under Omnomnom.

   All options (such as the random seed) may be configured via the command line.
   *)
(** Construct a test from a QCheck test. *)
val of_qcheck : QCheck.Test.t -> Omnomnom.Tests.test Omnomnom.Tests.tree
