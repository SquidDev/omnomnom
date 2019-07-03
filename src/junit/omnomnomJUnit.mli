(** A test reporter for Omnomnom which produces JUnit Xml files.

    The reporter does nothing by default, and must be turned on by the [--junit] flag.

    {2 Usage}

    {[
  Omnomnom.run
    ~reporter:
      Omnomnom.Ingredients.(compose_reporters (module ConsoleReporter) (module OmnomnomJUnit))
  @@ group "omnomnom"
    ]} *)

include Omnomnom.Ingredients.Reporter
