open Omnomnom.Tests

(** Construct an Alcotest test case, and convert it into an Omnomnom test. *)
val mk_alcotest_case : string -> Alcotest.speed_level -> (unit -> unit) -> tests

(** Create a test from a single Alcotest test case. *)
val of_alcotest_case : unit Alcotest.test_case -> tests

(** Create a test from a single Alcotest test. *)
val of_alcotest : unit Alcotest.test -> tests

(** Create a test from zero or many Alcotest tests. *)
val of_alcotests : string -> unit Alcotest.test list -> tests
