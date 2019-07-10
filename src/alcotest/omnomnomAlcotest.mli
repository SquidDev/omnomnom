open Omnomnom.Tests

(** Create a test from a single Alcotest test case. *)
val of_alcotest_case : unit Alcotest.test_case -> test tree

(** Create a test from a single Alcotest test. *)
val of_alcotest : unit Alcotest.test -> test tree

(** Create a test from zero or many Alcotest tests. *)
val of_alcotests : string -> unit Alcotest.test list -> test tree
