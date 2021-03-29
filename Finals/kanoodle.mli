open Kanoodle_types

(** Create a piece from a character label and a 
    list of locations. *)
val make_piece : char * loc list -> piece

(** Create a piece from a character label and a 
    string representation. *)
val make_piece_from_string : char * string -> piece

(** Print a representation of a piece to the terminal.
    For debugging. *)
val show_piece : piece -> unit

(** Move a piece a certain number of rows and columns. *)
val translate_piece : piece -> int -> int -> piece

(** Move a piece's coordinates so that the coordinates are as
    small as possible without changing the shape of the piece. *)
val normalize_piece : piece -> piece

(** Reflect a piece around the Y (column = 0) axis. *)
val reflect_piece : piece -> piece

(** Rotate a piece clockwise. *)
val rotate_piece : piece -> piece

(** Create a set of normalized pieces from a single piece. *)
val piece_in_all_orientations : piece -> PieceSet.t

(** Create a set of all (normalized) pieces in all orientations,
    given a list of (char, string) tuples, each of which 
    represents a piece. *)
val all_normalized_pieces : (char * string) list -> PieceSet.t

(** Return true if a piece's locations fit on a board. *)
val on_board : piece -> board -> bool

(** Translate a piece to every possible valid location on a board.
    Return a set of the resulting pieces. *)
val translate_piece_on_board : board -> piece -> PieceSet.t

(** Given a list of pieces in the (char, string) representation
    and a board, translate all pieces to all valid locations on a board. 
    Return a set of the resulting pieces. *)
val all_pieces_on_board : (char * string) list -> board -> PieceSet.t

(** Create an array of all the possible piece placements on a board. *)
val make_piece_array : (char * string) list -> board -> piece array

(** Create a list of all locations i.e. (row, column) coordinates on a board. *)
val all_locs_on_board : board -> loc list

(** Create an array of all the constraints on a board.
    Location constraints come first, then label constraints. *)
val make_constraints_array : (char * string) list -> board -> kconstraint array

(** Create a map between kconstraints and array indices. *)
val make_constraints_map : (char * string) list -> board -> int KconstraintMap.t

(** Given a list of pieces in the (char, string) representation
    and a board, generate a set of all (x, y) pairs from arrays of pieces 
    and constraints suitable for input to algorithm X. *)
val make_binary_matrix_base_locs : (char * string) list -> board -> LocSet.t

(** Same as `make_binary_matrix_base_locs` but (optionally) including
    extra row constraints. *)
val make_binary_matrix_locs : (char * string) list -> board -> LocSet.t

