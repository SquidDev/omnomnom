open Core
open Tests

module type Reporter = sig
  include Configurable

  val run : options -> (status Signal.sink tree -> result tree -> unit) option
end

type reporter = (module Reporter)
