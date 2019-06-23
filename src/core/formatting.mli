(** All output within Omnomnom is done using the {!Format} module. This allows different test
    reporters to customise the output, and apply different styles, while still providing a
    consistent frontend to the tests themselves.

    The {!Formatting} module provides some additional helpers for working with formatters, largely
    providing ways of displaying styled text. *)

(** One of the 8 basic colors supported by ANSI escape sequences. *)
type ansi_color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

(** A custom style to display text.

    While styles are based on those supported by ANSI escape sequences, they may be converted to
    other formats (such as HTML tags, etc...).

    This is a {!Format.stag}, and so can either be manually pushed and popped, or one can use the
    {!styled} and {!printf} helper functions. *)
type style =
  | Unstyled  (** Remove any styling from this string. *)
  | DullColor of ansi_color
      (** Tint the styled string's foreground with a "dull" color.

          When unit ANSI, this will be in the 30-37 range without the bold flag. *)
  | BrightColor of ansi_color
      (** Tint the styled string's foreground with a "bright" color.

          When unit ANSI, this will be in the 30-37 range with the bold flag. *)
  | Underlined  (** Underline this string. *)
  | Styled of style list  (** Apply multiple styles. *)

type Format.stag += Style of style

(** Call some printing function with the given style applied. *)
val styled : style -> Format.formatter -> (unit -> unit) -> unit

(** Print some formatting string with the given style applied.

    One must be careful to not partially apply this function, otherwise the formatting tag may be
    pushed once, but popped multiple times. *)
val printf : style -> Format.formatter -> ('a, Format.formatter, unit) format -> 'a
