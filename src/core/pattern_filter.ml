type options = string list

let options =
  let open Cmdliner.Arg in
  value & opt_all string []
  & info ~doc:"Filter tests to those matching any of these patterns." [ "p"; "pattern" ]

let prefix ~pre s start =
  let len = String.length pre in
  assert (len <= String.length s - start);
  let rec check i =
    if i = len then true
    else if String.unsafe_get s (i + start) <> String.unsafe_get pre i then false
    else check (i + 1)
  in
  check 0

let contains ~sub whole =
  let len = String.length sub in
  let max = String.length whole - len in
  let rec go i = if i > max then false else if prefix ~pre:sub whole i then true else go (i + 1) in
  go 0

let filter ps tests =
  let any name = List.exists (fun f -> contains ~sub:f name) ps in
  let rec go : 'a Tests.tree -> 'a Tests.tree option = function
    | TestCase (name, test) -> if any name then Some (TestCase (name, test)) else None
    | TestGroup (name, tests) -> (
        if any name then Some (TestGroup (name, tests))
        else
          match List.filter_map go tests with
          | [] -> None
          | tests -> Some (TestGroup (name, tests)))
  in
  match ps with
  | [] -> tests
  | _ -> (
    match go tests with
    | Some tests -> tests
    | None ->
        let (TestGroup (name, _) | TestCase (name, _)) = tests in
        TestGroup (name, []))

let results _ _ = ()
