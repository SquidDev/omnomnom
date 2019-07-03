open Omnomnom.Tests

let () =
  Omnomnom.run
  @@ group "omnomnom"
       [ group "Can perform golden tests"
           [ (let work ~name contents =
                Lwt.return
                @@
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
             pending "A test which needs to be written." ()
           ]
       ]
