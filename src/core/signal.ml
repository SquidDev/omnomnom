type 'a t =
  { mutable value : 'a;
    mutable listeners : ('a -> 'a -> unit) list option
  }

type 'a source = 'a t
type 'a sink = 'a t

let create x =
  let event = { value = x; listeners = Some [] } in
  (event, event)

let get { value; _ } = value

let on sink listener =
  match sink.listeners with
  | None -> ()
  | Some fs -> sink.listeners <- Some (listener :: fs)

let plug source = source.listeners <- None

let update source new_value =
  let old_value = source.value in
  source.value <- new_value;
  match source.listeners with
  | None -> ()
  | Some fs -> List.iter (fun f -> f old_value new_value) fs
