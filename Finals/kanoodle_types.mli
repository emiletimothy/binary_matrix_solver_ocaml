(** (Row, column) location on a board. *)
type loc = int * int

(** Sets of characters. *)
module CharSet : Set.S with type elt = char

(** Sets of locations. *)
module LocSet : Set.S with type elt = loc

(** Kanoodle pieces. *)
type piece = { label : char; locs : LocSet.t; }

(** Sets of Kanoodle pieces. *)
val piece_compare : piece -> piece -> int
module PieceSet : Set.S with type elt = piece

(** Board on which pieces are placed. *)
type board = { nrows : int; ncols : int; }

(** Kanoodle problem constraints.
    Each solution must occupy each location on the board exactly once
    and have a piece of each label represented exactly once. *)
type kconstraint = Loc of loc | Label of char

(** Map using kconstraints as keys. *)
module KconstraintMap : Map.S with type key = kconstraint

(** Map using locs as keys. *)
module LocMap : Map.S with type key = loc
