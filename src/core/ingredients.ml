open Core
open Tests

module type Reporter = sig
  include Configurable

  val run : options -> (status Signal.sink tree -> result tree -> unit) option
end

type reporter = (module Reporter)

module type Filter = sig
  include Core.Configurable

  val filter : options -> 'a Tests.tree -> 'a Tests.tree

  val results : options -> Tests.result Tests.tree -> unit
end

type filter = (module Filter)
