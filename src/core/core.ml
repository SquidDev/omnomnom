module type Configurable = sig
  type options

  val options : options Cmdliner.Term.t
end
