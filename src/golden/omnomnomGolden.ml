open Omnomnom.Tests

type process = name:string -> string -> string
type opts = { regenerate : bool } [@@unbox]

let options =
  let open Cmdliner in
  let open Cmdliner.Arg in
  let regenerate =
    value & flag
    & info ~docs:"Golden tests" ~doc:"Regenerate golden files on mismatch." [ "regenerate"; "r" ]
  in
  Term.(const (fun regenerate -> { regenerate }) $ regenerate)

let read_all channel =
  let buffer = Buffer.create 1024 in
  let bytes = Bytes.create 8192 in
  let rec read () =
    let n = input channel bytes 0 8192 in
    Buffer.add_subbytes buffer bytes 0 n;
    if n > 0 then read ()
  in
  let () = read () in
  close_in channel; Buffer.contents buffer

let compare ~regenerate ~directory ~output_name ~output_actual =
  let output_path = Filename.concat directory output_name in
  let exists = Sys.file_exists output_path in
  let output_expected = if exists then open_in output_path |> read_all else "" in
  if exists && output_actual = output_expected then result Pass
  else
    (* Generate a diff and pretty-print it. This is horrible, and really should be using some
       pretty-printing library. *)
    let diff =
      Diff.diff
        (String.split_on_char '\n' output_expected |> Array.of_list |> Diff.Slice.of_array)
        (String.split_on_char '\n' output_actual |> Array.of_list |> Diff.Slice.of_array)
    in
    (* Regenerate the file if needed .*)
    let () =
      if regenerate then (
        let channel = open_out output_path in
        output_string channel output_actual;
        close_out channel)
    in
    result ~message:(Fun.flip Diff.pp_diff diff) (Failed { backtrace = None })

let run ~regenerate ~action ~directory ~input_name ~output_name =
  let input_contents = Filename.concat directory input_name |> open_in |> read_all in
  let output_actual = action ~name:input_name input_contents in
  compare ~regenerate ~directory ~output_name ~output_actual

let make name action =
  test_case name
    (module struct
      type options = opts

      let options = options
      let run = action
    end : Test)

let of_output action ~directory ~output_name =
  make output_name (fun { regenerate } ->
      let output_actual = action ~name:output_name in
      compare ~regenerate ~directory ~output_name ~output_actual)

let of_file action ~directory ~input_name ~output_name =
  make input_name (fun { regenerate } ->
      run ~regenerate ~action ~directory ~input_name ~output_name)

let out x = x ^ ".out"

let of_directory action ?group:group_name ?(rename = out) ~directory ~extension () =
  match Sys.readdir directory with
  | names ->
      Array.to_list names
      |> List.filter (fun c -> Filename.extension c = extension)
      |> List.sort String.compare
      |> List.map (fun child ->
             of_file action ~directory ~input_name:child
               ~output_name:(Filename.remove_extension child |> rename))
      |> group (Option.value group_name ~default:directory)
  | exception e ->
      let res = result_of_exn e in
      test directory (fun () -> res)
