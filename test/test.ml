open Omnomnom.Tests
open OmnomnomAlcotest

(* The tests *)

let () =
  Omnomnom.run
    ~reporter:
      Omnomnom.Ingredients.(compose_reporters (module ConsoleReporter) (module OmnomnomJUnit))
  @@ group "omnomnom"
       [ group "Can perform golden tests"
           [ (let work ~name contents =
                if name = "fail.txt" then
                  contents ^ "\nOh no " ^ Printf.sprintf "%08x" (Random.bits ())
                else contents ^ "\nOh yes!"
              in
              OmnomnomGolden.of_directory work ~directory:"data/golden" ~extension:".txt" ())
           ];
         group "Some arbitrary tests"
           [ simple_test "Will assert a value" (fun () ->
                 assert (List.hd [ 0 ] = 0);
                 ());
             simple_test "Will fail" (fun () ->
                 let _ = List.hd [] in
                 ());
             simple_test "Will display output" (fun () ->
                 Printf.printf "Hello from stdout!\n";
                 Printf.eprintf "  and stderr.\n";
                 ());
             pending "A test which needs to be written." ()
           ];
         group "QCheck tests"
           (List.map OmnomnomQCheck.of_qcheck
              [ QCheck.Test.make ~count:1000 ~name:"list_rev_is_involutive"
                  QCheck.(list small_nat)
                  (fun l -> List.rev (List.rev l) = l);
                QCheck.Test.make ~count:1000 ~name:"my_buggy_test"
                  QCheck.(list small_nat)
                  (fun l -> List.rev l = l)
              ]);
         (let module To_test = struct
            let capit letter = Char.uppercase_ascii letter

            let plus int_list = List.fold_left (fun a b -> a + b) 0 int_list
          end in
         let capit () =
           Alcotest.(check char) "same chars" 'A' (To_test.capit 'a');
           ()
         in
         let plus () =
           Alcotest.(check int) "same ints" 8 (To_test.plus [ 1; 1; 2; 3 ]);
           ()
         in
         let fails () = Alcotest.fail "Oh no" in
         of_alcotest
           ( "Alcotests",
             [ ("Capitalize", `Quick, capit);
               ("Add entries", `Slow, plus);
               ("Fails", `Quick, fails)
             ] ))
       ]
