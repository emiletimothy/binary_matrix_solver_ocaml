open Kanoodle_types
open Kanoodle

(** Validate a solution given an array of pieces and a list of indices. *)
val validate_solution : board -> piece array -> int list -> bool

(** Convert a solution to a string suitable for printing. *)
val string_of_solution : board -> piece array -> int list -> string

