module type Configurable = sig
  type options

  val options : options Cmdliner.Term.t
end

module NoConfiguration = struct
  type options = unit

  let options = Cmdliner.Term.const ()
end
