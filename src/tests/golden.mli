open OmnomnomCore.Tests

type process = name:string -> string -> string Lwt.t

val of_file : process -> directory:string -> input_name:string -> output_name:string -> test tree

val of_directory :
  process -> ?rename:(string -> string) -> directory:string -> extension:string -> test tree
