open Omnomnom.Tests
module Q = QCheck2.Test
module R = QCheck2.TestResult

type opts = { seed : int } [@@unbox]

let options =
  let open Cmdliner in
  let open Cmdliner.Arg in
  let seed =
    value & opt (some int) None & info ~docs:"QCheck" ~doc:"The initial seed to use" [ "q-seed" ]
  in
  let get_seed = function
    | Some x -> x
    | None -> Random.self_init (); Random.int 1_000_000_000
  in
  Term.(const (fun seed -> { seed = get_seed seed }) $ seed)

let rec pp_list fmt f = function
  | [] -> ()
  | [ x ] -> Format.fprintf f "%a@," fmt x
  | x :: y -> Format.fprintf f "%a@,%a" fmt x (pp_list fmt) y

let of_qcheck (Q.Test cell) =
  test_case (Q.get_name cell)
    (module struct
      type options = opts

      let options = options

      let run { seed } =
        let rand = Random.State.make [| seed |] in
        let r = Q.check_cell cell ~rand in
        let count = R.get_count r and state = R.get_state r in
        let print_failure f { R.instance; shrink_steps; msg_l } =
          Format.fprintf f "%s" (Q.print_instance cell instance);
          if shrink_steps > 0 then Format.fprintf f " (after %d shrink steps)" shrink_steps;
          List.iter (Format.fprintf f "%s\n") msg_l
        in
        match state with
        | Success -> result ~message:(fun f -> Format.fprintf f "%d trials completed." count) Pass
        | Failed { instances } ->
            result
              ~message:(fun f ->
                Format.fprintf f "@[Failed after %d trials on ≥ %d cases:@ @[<v>%a@]@]" count
                  (List.length instances) (pp_list print_failure) instances)
              (Failed { backtrace = None })
        | Error { instance; exn; backtrace } ->
            result
              ~message:(fun f ->
                Format.fprintf f "Errored after %d trials : %s\n%s\n%a" count
                  (Printexc.to_string exn) backtrace print_failure instance)
              (Errored { backtrace = None })
        | Failed_other { msg } ->
            result ~message:(fun f -> Format.fprintf f "%s" msg) (Errored { backtrace = None })
    end : Test)
