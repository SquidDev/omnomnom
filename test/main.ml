open Omnomnom.Tests

let () =
  Omnomnom.run ~reporter:(module Omnomnom.Ingredients.ConsoleReporter)
  @@ group "omnomnom"
       [ (let work ~name old_contents =
            let open OmnomnomGolden__.Diff in
            let new_contents =
              Filename.remove_extension name |> Printf.sprintf "data/diff/%s.new" |> open_in
              |> Omnomnom__.Io_util.read_all
            in
            diff
              (String.split_on_char '\n' old_contents |> Array.of_list |> Slice.of_array)
              (String.split_on_char '\n' new_contents |> Array.of_list |> Slice.of_array)
            |> Format.asprintf "%a" pp_diff
          in
          OmnomnomGolden.of_directory work
            ~rename:(fun x -> x ^ ".diff")
            ~group:"Generates diffs" ~directory:"data/diff" ~extension:".old" ())
       ]
