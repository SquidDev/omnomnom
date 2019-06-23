type ansi_color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

type style =
  | Unstyled
  | DullColor of ansi_color
  | BrightColor of ansi_color
  | Underlined
  | Styled of style list

type Format.stag += Style of style

let styled style out fn =
  Format.pp_open_stag out (Style style);
  fn ();
  Format.pp_close_stag out ()

let printf style out fmt =
  Format.pp_open_stag out (Style style);
  Format.kfprintf (fun x -> Format.pp_close_stag x ()) out fmt
