open Core
open Tests

module type Reporter = sig
  include Configurable

  val run : options -> (status Signal.sink tree -> result tree -> unit) option
end

type reporter = (module Reporter)

let compose_reporters left right =
  let module Left = (val left : Reporter) in
  let module Right = (val right : Reporter) in
  ( module struct
    type options = Left.options * Right.options

    let options =
      let open Cmdliner.Term in
      const (fun a b -> (a, b)) $ Left.options $ Right.options

    let run (a, b) =
      match (Left.run a, Right.run b) with
      | None, None -> None
      | Some x, None | None, Some x -> Some x
      | Some a, Some b ->
          Some
            (fun p ->
              let a = a p and b = b p in
              fun r -> a r; b r)
  end : Reporter )
