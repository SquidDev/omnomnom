open Omnomnom.Tests

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
              OmnomnomGolden.of_directory work ?rename:None ~directory:"data/golden"
                ~extension:".txt")
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
              ])
       ]
