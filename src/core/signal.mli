(** Provides a continuously updating data point, and a way to monitor changes to it.

    Similar to React's signal implementation, but home grown because of course it is. *)

(** A source of data.

    This is used to fire changes to the {!sink}. *)
type 'a source

(** A sink into which signal changes are put.

    This is used to subscribe to changes fired from the corresponding {!source}. *)
type 'a sink

(** Create a new event sink and source. *)
val create : 'a -> 'a source * 'a sink

(** Get the current value of this sink. *)
val get : 'a sink -> 'a

(** Register a function to call whenever a sink changes value. Called with the old and new values. *)
val on : 'a sink -> ('a -> 'a -> unit) -> unit

(** "Plug" this source, preventing any further events from being fired, and removing all event
    listeners.

    Attempting to subscribe to a plugged sink, or fire an event on a plugged source will do
    nothing. *)
val plug : 'a source -> unit

(** Update the value of this source, notifying any subscribed listeners. *)
val update : 'a source -> 'a -> unit
