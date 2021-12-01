(** A port of simplediff[1] to OCaml. This is an incredibly poor diff implementation (missing patch
    support), terrible complexity, but hopefully it's good enough for the time being.

    [1]: https://github.com/paulgb/simplediff *)
module Slice = struct
  type 'a t =
    { array : 'a array;
      start : int;
      length : int
    }

  let of_array array = { array; start = 0; length = Array.length array }

  let length x = x.length

  let iter f { array; start; length } =
    for i = 0 to length - 1 do
      f array.(start + i)
    done

  let iteri f { array; start; length } =
    for i = 0 to length - 1 do
      f i array.(start + i)
    done

  let slice ~from ~length { array; start; _ } = { array; start = start + from; length }
end

type 'a diff =
  | Add of 'a Slice.t
  | Remove of 'a Slice.t
  | Keep of 'a Slice.t
  | Seq of 'a diff list

let rec diff olds news =
  match (Slice.length olds, Slice.length news) with
  | 0, 0 -> Seq []
  | 0, _ -> Add news
  | _, 0 -> Remove olds
  | _, _ ->
      (* Build up a map of all lines contents to their positions. *)
      let old_index_map = Hashtbl.create (Slice.length olds / 2) in
      Slice.iteri (fun i x -> Hashtbl.add old_index_map x i) olds;

      let overlap = Array.make (Slice.length olds) 0 in
      let sub_length = ref 0 and sub_range = ref (0, 0) in

      news
      |> Slice.iteri (fun new_index value ->
             let this_overlap = Array.make (Slice.length olds) 0 in
             Hashtbl.find_all old_index_map value
             |> List.iter (fun old_index ->
                    let this = if old_index = 0 then 1 else overlap.(old_index - 1) + 1 in
                    this_overlap.(old_index) <- this;
                    if this > !sub_length then (
                      sub_length := this;
                      sub_range := (old_index - this + 1, new_index - this + 1)));
             Array.blit this_overlap 0 overlap 0 (Array.length this_overlap));

      if !sub_length = 0 then Seq [ Remove olds; Add news ]
      else
        let old_idx, new_idx = !sub_range and length = !sub_length in
        Seq
          [ diff
              (Slice.slice ~from:0 ~length:old_idx olds)
              (Slice.slice ~from:0 ~length:new_idx news);
            Keep (Slice.slice ~from:new_idx ~length news);
            diff
              (Slice.slice ~from:(old_idx + length)
                 ~length:(Slice.length olds - old_idx - length)
                 olds)
              (Slice.slice ~from:(new_idx + length)
                 ~length:(Slice.length news - new_idx - length)
                 news)
          ]

let fold ~add ~remove ~keep diff =
  let rec go = function
    | Add x -> add x
    | Remove x -> remove x
    | Keep x -> keep x
    | Seq xs -> List.iter go xs
  in
  go diff

let pp_diff out diff =
  let module F = Omnomnom.Formatting in
  (* The eta-expansion here is important, otherwise the formatting codes are only emitted once. *)
  let keep = Slice.iter (fun x -> Format.fprintf out " %s@\n" x)
  and remove = Slice.iter (fun x -> F.printf F.(DullColor Red) out "-%s@\n" x)
  and add = Slice.iter (fun x -> F.printf F.(DullColor Green) out "+%s@\n" x) in
  fold ~keep ~add ~remove diff
