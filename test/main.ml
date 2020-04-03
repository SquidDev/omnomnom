open Omnomnom.Tests

let () = Omnomnom.run @@ group "omnomnom" [ Golden_diff.tests; Pattern_filter.tests ]
