open Kanoodle_types
open Kanoodle

(*
 * Validating solutions.
 *)

(*
 * Validate a solution.
 *
 * Solution criteria:
 *   -- no labels are repeated
 *   -- the union of all the piece locations covers the entire board
 *   -- no pieces overlap (no repeated piece locations)
 *
 * Arguments:
 *   board:   the board being solved
 *   pieces:  an array of pieces, including all possible configurations of
 *            all pieces
 *   indices: the indices of the piece configurations that solve the board
 *)
let validate_solution board pieces indices =
  (* Return true if the set of locations covers all locations on the board. *)
  let covers board set =
    let rec iter row col =
      match () with
        | _ when row = board.nrows -> true  (* done checking *)
        | _ when col = board.ncols -> iter (row + 1) 0
        | _ -> if LocSet.mem (row, col) set then iter row (col + 1) else false
    in iter 0 0
  in
  let alen            = Array.length pieces in
  (* Indices that are too large are due to extra rows added to
     allow pieces to be omitted. *)
  let indices'        = List.filter (fun n -> n < alen) indices in
  let solution_pieces = List.map (fun ix -> pieces.(ix)) indices' in
  let solution_locs   = List.map (fun p -> p.locs) solution_pieces in
  let all_locs        = List.fold_left LocSet.union LocSet.empty solution_locs in
  let all_locs_list   = List.concat (List.map LocSet.elements solution_locs) in
  let labels          = List.map (fun p -> p.label) solution_pieces in
  let labelset        = CharSet.of_list labels in
    List.length labels = CharSet.cardinal labelset
      && List.length all_locs_list = LocSet.cardinal all_locs
      && covers board all_locs

(* Convert a solution to a string suitable for printing.
 *
 * Arguments:
 *   board:   the board being solved
 *   pieces:  an array of pieces, including all possible configurations of
 *            all pieces
 *   indices: the indices of the piece configurations that solve the board
 *)
let string_of_solution board pieces indices =
  (* Get the pieces which are part of the solution. *)
  let alen            = Array.length pieces in
  (* Indices that are too large are due to extra rows added to
     allow pieces to be omitted. *)
  let indices'        = List.filter (fun n -> n < alen) indices in
  let solution_pieces = List.map (fun ix -> pieces.(ix)) indices' in
  (* Convert the solution pieces list to a map between locations and labels. *)
  let map =
    let convert_piece m p =
      let locs = LocSet.elements p.locs in
        List.fold_left (fun m l -> LocMap.add l p.label m) m locs
    in
      List.fold_left convert_piece LocMap.empty solution_pieces
  in
  let rec iter row col s =
    match () with
      | _ when row = board.nrows -> s
      | _ when col = board.ncols -> iter (row + 1) 0 (s ^ "\n")
      | _ ->
        (* This should never fail (throw a Not_found exception).
         * N.B. Char.escaped converts the label char to a
         * one-character string (for all labels we use). *)
        let label = LocMap.find (row, col) map in
          iter row (col + 1) (s ^ Char.escaped label)
  in iter 0 0 ""

