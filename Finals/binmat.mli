module IntSet : Set.S with type elt = int
module IntMap : Map.S with type key = int

module type BinaryMatrix =
  sig
    type t  (* type of binary matrices *)

    (* constructor: nrows, ncols, (row, col) pairs where value = 1 -> matrix *)
    val make : int -> int -> (int * int) list -> t  

    val nrows : t -> int  (* number of rows *)
    val ncols : t -> int  (* number of columns *)

    (* These functions return a list of all "active" row or column
       indices.  By "active" we mean indices of rows or columns
       which initially contained nonzero entries and where the
       row or column has not explicitly been deleted yet. 
       Note that such rows or columns can still be empty. *)
    val rows : t -> int list
    val cols : t -> int list

    (* Get a value given its row/column indices. 
       For debugging/testing only. *)
    val get : t -> int -> int -> bool  

    (* Print a string representation of the board to the terminal. *)
    val dump : t -> unit

    (* for a given column c, return a list of rows r for which A(r, c) = 1 *)
    val rows_for_col : t -> int -> int list

    (* for a given row r, return a list of columns c for which A(r, c) = 1 *)
    val cols_for_row : t -> int -> int list

    (* Delete a row or column.  This means to remove all of its entries
       and make it so that calling "rows" (for rows) or "cols" (for columns)
       doesn't return this row/column index.  Note that a row or column
       can be empty without being "deleted".  Being empty means there are
       no 1s in that row or column, whereas being deleted means that that
       row or column is not part of the matrix anymore. *)
    val delete_row : t -> int -> t
    val delete_col : t -> int -> t

    (* return the index of the column with the minimum number of 1s *)
    val min_col_sum_index : t -> int  
  end

module ImplBinaryMatrix : BinaryMatrix

