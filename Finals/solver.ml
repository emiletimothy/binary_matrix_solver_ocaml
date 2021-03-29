open Binmat

(* Algorithm X:

    If the matrix A has no columns, the current partial solution is a valid
      solution; terminate successfully.s

    Otherwise choose a column c (deterministically).  Ideally this should be
    the column with the fewest 1s in it.
   
    Choose a row r such that A(r, c) = 1 (nondeterministically).  If there is no
    such row (i.e. column c is all 0s), the algorithm terminates unsuccessfully.
    Otherwise, include row r in the partial solution.

    Given row r:
      for each column c such that m(r, c) = 1,
        for each row r' such that m(r', c) = 1,
          delete row r from matrix A.
        delete column c from matrix A.

    Repeat this algorithm recursively on the reduced matrix A.
 *)

(* Utility function *)
let list_shuffle lst =
  let array_shuffle a =
    let n = Array.length a in
    let a = Array.copy a in
      begin
        for i = n - 1 downto 1 do
          let k = Random.int (i+1) in
          let x = a.(k) in
          a.(k) <- a.(i);
          a.(i) <- x
        done;
        a
      end
  in
    lst |> Array.of_list |> array_shuffle |> Array.to_list

module AlgorithmX(B : BinaryMatrix) =
  struct

    let rec remove_rows_in_c (matrix : B.t) c = 
      let rows = B.rows_for_col matrix c in
      match rows with
        | [] -> matrix
        | r::t -> remove_rows_in_c (B.delete_row matrix r) c

    let deleter row (matrix : B.t) = let rec each_column cols_list matrix = 
      match cols_list with
        | [] -> matrix
        | c::t -> B.delete_col (each_column t (remove_rows_in_c matrix c)) c
      in each_column (B.cols_for_row matrix row) matrix

    let rec helper_solve (matrix : B.t) set = 
      if B.cols matrix = [] then (Some set) 
      else let min_column = B.min_col_sum_index matrix in
        let rows = list_shuffle (B.rows_for_col matrix min_column) in
        let rec row_thing rows matrix set =
          match rows with 
            | [] -> None
            | h::t -> let new_set = helper_solve (deleter h matrix) (IntSet.add h set) in
              match new_set with
                | Some s -> Some s
                | None -> row_thing t matrix (IntSet.remove h set)
        in row_thing rows matrix set

    let solve (matrix : B.t) : IntSet.t option =
      let set = IntSet.empty in
      let b = helper_solve matrix set in
      match b with
        | None -> None
        | Some s -> Some s

  end;;

