open Omnomnom
open Omnomnom.Tests

let () =
  run
  @@ group "omnomnom"
       [ group "Can perform golden tests"
           [ (let work ~name contents =
                Lwt.return
                @@
                if name = "fail.txt" then
                  contents ^ "\nOh no " ^ Printf.sprintf "%08x" (Random.bits ())
                else contents ^ "\nOh yes!"
              in
              Tests.Golden.of_directory work ?rename:None ~directory:"test/data/golden"
                ~extension:".txt")
           ];
         group "Some arbitrary tests"
           [ simple_test "Will assert a value" (fun () ->
                 assert (List.hd [ 0 ] = 0);
                 () );
             simple_test "Will fail" (fun () ->
                 let _ = List.hd [] in
                 () );
             pending "A test which needs to be written." ()
           ]
       ]
