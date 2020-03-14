open Tests
module S = Signal
module F = Formatting

type display =
  | Tests
  | NoisyTests
  | Groups
  | Nothing

type options =
  { display : display;
    base_dir : string;
    timing : bool;
    colour : bool
  }

let options =
  let open Cmdliner in
  let open Cmdliner.Arg in
  let display =
    value
    & opt ~vopt:Tests
        (enum [ ("tests", Tests); ("groups", Groups); ("noisy", NoisyTests); ("none", Nothing) ])
        Nothing
    & info ~docs:"Console Reporter" ~doc:"What information to output when running tests."
        [ "display" ]
  in
  let base_dir_env =
    Term.env_info ~docs:"Console Reporter"
      ~doc:"The base directory from which to resolve source files." "OM_BASE_DIR"
  in
  let base_dir =
    value & opt string "."
    & info ~docs:"Console Reporter" ~doc:"The base directory from which to resolve source files."
        ~env:base_dir_env [ "base-dir" ]
  in
  let timing =
    value & flag
    & info ~docs:"Console Reporter" ~doc:"Whether to display timings along with the test output."
        [ "time"; "t" ]
  in
  let colour =
    value & flag
    & info ~docs:"Console Reporter" ~doc:"Whether to use colours in the test output."
        [ "no-colour"; "C" ]
  in
  Term.(
    const (fun display timing colour base_dir -> { display; timing; colour; base_dir })
    $ display $ timing
    $ (const not $ colour)
    $ base_dir)

let get_dot result =
  match result with
  | Pass -> F.(DullColor Green, "•")
  | Skipped -> F.(DullColor Yellow, "○")
  | Failed _ -> F.(DullColor Red, "■")
  | Errored _ -> F.(DullColor Magenta, "✱")

(** Set up a printer for colour.

    This just wraps the current stag function table with one which emits ANSI codes.*)
let setup_color_printer out =
  let prev = Format.pp_get_formatter_stag_functions out () in
  let stack = ref [ "\027[0m" ] in
  let get_color = function
    | F.Black -> 0
    | Red -> 1
    | Green -> 2
    | Yellow -> 3
    | Blue -> 4
    | Magenta -> 5
    | Cyan -> 6
    | White -> 7
  in
  let rec get_style = function
    | F.Unstyled -> "\027[0m"
    | Underlined -> "\027[0m"
    | DullColor c -> Printf.sprintf "\027[3%dm" (get_color c)
    | BrightColor c -> Printf.sprintf "\027[1;3%dm" (get_color c)
    | Styled c -> List.map get_style c |> String.concat ""
  in
  Format.pp_set_mark_tags out true;
  Format.pp_set_margin out 120;
  Format.pp_set_formatter_stag_functions out
    { prev with
      mark_open_stag =
        (function
        | F.Style s ->
            let style = get_style s in
            stack := style :: !stack;
            style
        | x -> prev.mark_open_stag x);
      mark_close_stag =
        (function
        | F.Style _ ->
            let style =
              match !stack with
              | _ :: (x :: _ as xs) ->
                  stack := xs;
                  x
              | _ -> failwith "Popping from an empty stack"
            in
            "\027[0m" ^ style
        | x -> prev.mark_close_stag x)
    }

(** Attempt to resolve source code for the erroring function, and displays that along with a
    backtrace. *)
let format_trace ~base_dir ~(out : Format.formatter) x =
  let ( >>= ) x f =
    match x with
    | None -> None
    | Some x -> f x
  in
  let rec times f i =
    if i <= 0 then []
    else
      let x = f () in
      x :: times f (i - 1)
  in
  Printexc.backtrace_slots x
  >>= Array.fold_left
        (fun head slot ->
          (* Find a file from which we can read the appropriate lines. *)
          match (head, Printexc.Slot.location slot) with
          | None, Some loc -> (
            try
              let filename = loc.filename in
              let filename =
                if Filename.is_relative filename then Filename.concat base_dir filename
                else filename
              in
              let ch = open_in filename in
              let res =
                try
                  for _ = 1 to loc.line_number - 4 do
                    input_line ch |> ignore
                  done;
                  let head = times (fun () -> input_line ch) 3 in
                  let line = input_line ch in
                  let tail =
                    times (fun () -> try Some (input_line ch) with End_of_file -> None) 3
                  in
                  Some (loc.line_number, head, line, tail)
                with End_of_file -> None
              in
              close_in ch; res
            with Sys_error _ -> None )
          | x, _ -> x)
        None
  >>= (fun (line, before, contents, after) ->
        (* And read them! *)
        let padding = String.make (String.length (string_of_int line)) ' ' in
        List.iter (fun x -> Format.fprintf out "%s | %s@\n" padding x) before;
        F.printf F.(DullColor Red) out "%d | %s@\n" line contents;
        List.iter
          (function
            | None -> ()
            | Some l -> Format.fprintf out "%s | %s@\n" padding l)
          after;
        Format.pp_force_newline out ();
        None)
  |> ignore

let pp_indent out indent action =
  Format.pp_open_box out indent;
  Format.pp_print_string out (String.make indent ' ');
  action ();
  Format.pp_close_box out ()

let print_results { display; base_dir; _ } out results =
  let base_dir =
    if Filename.is_relative base_dir then Filename.concat (Unix.getcwd ()) base_dir else base_dir
  in
  Format.pp_print_newline out ();
  (* Now print the summary *)
  let is_success = function
    | Pass | Skipped -> true
    | Failed _ | Errored _ -> false
  in
  let format_test name { outcome; message; _ } =
    let show =
      match (is_success outcome, display) with
      | false, _ -> true (* Failing tests always show *)
      | true, Tests -> true (* Tests always show *)
      | true, NoisyTests -> Option.is_some message
      | true, (Nothing | Groups) -> false
    in
    ( (if is_success outcome then 1 else 0),
      1,
      if not show then None
      else
        (* Otherwise include the test name. *)
        Some
          (fun () ->
            let style, icon =
              match outcome with
              | Pass -> F.(DullColor Green, "✓")
              | Skipped -> F.(DullColor Yellow, "○")
              | Failed _ | Errored _ -> F.(DullColor Red, "✘")
            in
            F.printf style out "%s" icon;
            Format.fprintf out " %s" name;
            ( match message with
            | None -> ()
            | Some message ->
                Format.pp_force_newline out ();
                pp_indent out 2 (fun () -> message out) );
            match outcome with
            | Pass | Skipped -> ()
            | Failed e | Errored e -> (
              match e with
              | { backtrace = None } -> ()
              | { backtrace = Some backtrace } ->
                  Format.pp_force_newline out ();
                  pp_indent out 2 @@ fun () ->
                  (* Print the message and backtrace. *)
                  format_trace ~base_dir ~out backtrace;
                  Format.pp_force_newline out ();
                  Format.pp_print_text out (Printexc.raw_backtrace_to_string backtrace) )) )
  in
  let format_group name pass total children =
    ( pass,
      total,
      if List.length children = 0 && display = Nothing then
        (* Skip groups when they've no children and not showing groups. *)
        None
      else
        (* Otherwise display the group name and extra info. *)
        Some
          (fun () ->
            Format.fprintf out "%s " name;
            F.printf F.(BrightColor Cyan) out "(%d out of %d passed)@\n" pass total;
            pp_indent out 2 @@ fun () ->
            List.iteri
              (fun i f ->
                if i > 0 then Format.pp_force_newline out ();
                f ())
              children) )
  in
  let rec print_tests = function
    | TestCase (name, result) -> format_test name result
    | TestGroup (name, children) ->
        let pass, total, children =
          List.fold_right
            (fun child (pass, total, children) ->
              let p, t, c = print_tests child in
              ( pass + p,
                total + t,
                match c with
                | None -> children
                | Some x -> x :: children ))
            children (0, 0, [])
        in
        format_group name pass total children
  in
  let _, _, fn = print_tests results in
  match fn with
  | Some fn -> fn (); Format.pp_force_newline out ()
  | None -> ()

let run ({ colour; _ } as options) =
  Some
    (fun tests ->
      (* Set up our printer. *)
      let out = Format.std_formatter in
      if colour then setup_color_printer out;
      (* Print the progress of the tests as they come in. *)
      let print_progress_dot _ = function
        | Finished { outcome; _ } ->
            let style, dot = get_dot outcome in
            F.printf style out "%s" dot
        | _ -> ()
      in
      let rec print_progress = function
        | TestCase (_, p) -> S.on p print_progress_dot
        | TestGroup (_, tests) -> List.iter print_progress tests
      in
      print_progress tests;
      (* When we receive all results, print them *)
      print_results options out)
