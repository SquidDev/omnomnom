open Omnomnom.Tests
open OmnomnomAlcotest

let tree : unit tree Alcotest.testable =
  (module struct
    type t = unit tree

    let rec pp out = function
      | TestCase (name, _) -> Fmt.string out name
      | TestGroup (name, tests) ->
          Fmt.(fmt "%a[%a]" out string name (box (list ~sep:comma pp)) tests)

    let equal = ( = )
  end)

let filter patterns tests =
  let module M = (val Omnomnom.Ingredients.pattern_filter) in
  match
    Cmdliner.Term.(
      eval
        ~argv:(List.map (fun f -> "-p" ^ f) patterns |> List.cons "test" |> Array.of_list)
        (M.options, info "test"))
  with
  | `Ok patterns -> M.filter patterns tests
  | _ -> Alcotest.fail "Cannot parse command line"

let group name xs = TestGroup (name, xs)

let test name = TestCase (name, ())

let tests =
  group "Pattern filter"
  @@ [ mk_alcotest_case "Empty filter does nothing" `Quick (fun () ->
           Alcotest.check tree "Pattern filter"
             (group "a" [ test "b" ])
             (filter [] (group "a" [ test "b" ])));
       mk_alcotest_case "Pattern filter removes tests" `Quick (fun () ->
           Alcotest.check tree "Pattern filter"
             (group "a" [ test "b" ])
             (filter [ "b" ] (group "a" [ test "b"; test "c"; test "d" ])));
       mk_alcotest_case "Pattern filter allows multiple patterns " `Quick (fun () ->
           Alcotest.check tree "Pattern filter"
             (group "a" [ test "b"; test "c" ])
             (filter [ "b"; "c" ] (group "a" [ test "b"; test "c"; test "d" ])));
       mk_alcotest_case "Pattern filter includes groups" `Quick (fun () ->
           Alcotest.check tree "Pattern filter"
             (group "a" [ test "b"; test "c"; test "d" ])
             (filter [ "a" ] (group "a" [ test "b"; test "c"; test "d" ])));
       mk_alcotest_case "Pattern filter removes empty groups" `Quick (fun () ->
           Alcotest.check tree "Pattern filter"
             (group "a" [ test "c" ])
             (filter [ "c" ] (group "a" [ group "d" [ test "b" ]; test "c"; test "d" ])))
     ]
