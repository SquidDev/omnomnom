open OmnomnomCore.Tests

type process = name:string -> string -> string Lwt.t

type opts = { regenerate : bool } [@@unbox]

let options =
  let open Cmdliner in
  let open Cmdliner.Arg in
  let regenerate =
    value & flag
    & info ~docs:"Golden tests" ~doc:"Regenerate golden files on mismatch." [ "regenerate"; "r" ]
  in
  Term.(const (fun regenerate -> { regenerate }) $ regenerate)

let read_all input =
  let buffer = Buffer.create 1024 in
  let bytes = Bytes.create 8192 in
  let rec read () =
    let%lwt n = Lwt_io.read_into input bytes 0 8192 in
    Buffer.add_subbytes buffer bytes 0 n;
    if n > 0 then read () else Lwt.return ()
  in
  let%lwt () = read () in
  Lwt.return (Buffer.contents buffer)

let display_diff diff out =
  let module Diff = Patience_diff_lib.Patience_diff in
  let module F = OmnomnomCore.Formatting in
  (* The eta-expansion here is important, otherwise the formatting codes are only emitted once. *)
  let same x = Format.fprintf out "  %s@\n" x
  and minus x = F.printf F.(DullColor Red) out "- %s@\n" x
  and plus x = F.printf F.(DullColor Green) out "+ %s@\n" x in
  diff
  |> List.iter (fun (hunk : string Diff.Hunk.t) ->
         F.printf
           F.(DullColor Cyan)
           out "@@@@@ -%i,%i +%i,%i @@@@@@m@\n" hunk.prev_start hunk.prev_size hunk.next_start
           hunk.next_size;
         hunk.ranges
         |> List.iter (function
              | Diff.Range.Same xs -> xs |> Array.iter (fun (x, _) -> same x)
              | Prev xs -> Array.iter minus xs
              | Next xs -> Array.iter plus xs
              | Replace (prev, next) -> Array.iter minus prev; Array.iter plus next
              | Unified xs -> Array.iter same xs ) )

let run ~regenerate ~action ~directory ~input_name ~output_name =
  (* Read the input and output *)
  let input_path = Filename.concat directory input_name in
  let output_path = Filename.concat directory output_name in
  let input_contents = Lwt_io.with_file ~mode:Lwt_io.input input_path read_all in
  let output_expected =
    try%lwt Lwt_io.with_file ~mode:Lwt_io.input output_path read_all
    with Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return ""
  in
  (* And resolve the actual output + expected output. *)
  let%lwt output_actual = Lwt.bind input_contents (action ~name:input_name) in
  let%lwt output_expected = output_expected in
  if output_actual = output_expected then Lwt.return Pass
  else
    (* Generate a diff and pretty-print it. This is horrible, and really should be using some
       pretty-printing library. *)
    let module Diff = Patience_diff_lib.Patience_diff in
    let diff =
      Diff.String.get_hunks
        ~transform:(fun x -> x)
        ~context:3 ?big_enough:None
        ~prev:(String.split_on_char '\n' output_expected |> Array.of_list)
        ~next:(String.split_on_char '\n' output_actual |> Array.of_list)
    in
    (* Regenerate the file if needed .*)
    let%lwt () =
      if regenerate then
        Lwt_io.with_file ~mode:Lwt_io.output output_path (fun ch -> Lwt_io.write ch output_actual)
      else Lwt.return_unit
    in
    Lwt.return (Failed { message = display_diff diff; backtrace = None })

let of_file action ~directory ~input_name ~output_name =
  test_case input_name
    ( module struct
      type options = opts

      let options = options

      let run { regenerate } = run ~regenerate ~action ~directory ~input_name ~output_name
    end
    : Test )

let out x = x ^ ".out"

let of_directory action ?(rename = out) ~directory ~extension =
  try
    let names = ref [] in
    let handle = Unix.opendir directory in
    ( try
        while true do
          let child = Unix.readdir handle in
          if child <> "." && child <> ".." && Filename.extension child = extension then
            names := child :: !names
        done
      with End_of_file -> () );
    !names |> List.sort String.compare
    |> List.map (fun child ->
           of_file action ~directory ~input_name:child
             ~output_name:(Filename.remove_extension child |> rename) )
    |> group directory
  with Unix.Unix_error _ as e ->
    let res = result_of_exn e in
    test directory (fun () -> res)
