(** An object which can be configured via command line arguments or environment variables. *)
module type Configurable = sig
  (** The type of options this object accepts. *)
  type options

  (** The cmdliner term which will provide these options. *)
  val options : options Cmdliner.Term.t
end
