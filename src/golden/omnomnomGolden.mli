(** Golden tests are very similar to any other unit test - you run a function and compare the output
    against the expected output.

    However, under golden tests, the input and expected output are stored on the file system. This
    makes them more convenient when you're dealing with non-trivial inputs or outputs. Also, by
    storing files on disk, it makes it possible to automatically regenerate these golden files, and
    verify that those changes are what you would expect.

    See {{:https://ro-che.info/articles/2017-12-04-golden-tests} this article} for more information
    about golden testing. *)

open Omnomnom.Tests

(** Create a golden test file which has no input file, merely producing an output.

    The function will simply be applied with the same value as [output_name] *)
val of_output : (name:string -> string) -> directory:string -> output_name:string -> test tree

(** The function to apply to the input file's contents. Should simply return the computed result.

    Note, we also supply {e relative} file name - this should not be treated as a location on disk,
    but merely a useful name for error messages and the like. *)
type process = name:string -> string -> string

(** Create a golden test using using the name of the input file and the output "golden" file. *)
val of_file : process -> directory:string -> input_name:string -> output_name:string -> test tree

(** Create zero or more golden tests from files with a specific extension within the directory.

    One may optionally supply a rename function, which takes the filename (without the extension)
    and returns the name of the golden file. If not specified, we will look for a file with an
    [.out] extension.

    For instance, the following will use all [.json] files within the [test-data] directory as
    inputs.

    {[ of_directory ~process:do_something ~directory:"test-data" ~extension:"json" () ]} *)
val of_directory :
  process ->
  ?group:string ->
  ?rename:(string -> string) ->
  directory:string ->
  extension:string ->
  unit ->
  test tree
