(** A test reporter for Omnomnom which produces JUnit Xml files.

    The reporter does nothing by default, and must be turned on by the [--junit] flag.

    {2 Usage}

    {[
      Omnomnom.run
        ~reporter:Omnomnom.Ingredients.(compose_reporters console_reporter OmnomnomJUnit.reporter)
      @@ group "omnomnom"
    ]} *)

include Omnomnom.Ingredients.Reporter

(** An alias to this module, but as a value. *)
val reporter : Omnomnom.Ingredients.reporter
