open Kanoodle_pieces
open Kanoodle_types
open Kanoodle
open Kanoodle_utils
open Binmat
open Solver

module FindSolutions(B: BinaryMatrix) =
  struct
    module Alg = AlgorithmX(B)

    let get_solution piece_string_list board =
      let nrows = Array.length (make_piece_array piece_string_list board) in
      let ncols = Array.length (make_constraints_array piece_string_list board) in
      let coords_set = make_binary_matrix_locs piece_string_list board in
      let coords = LocSet.elements coords_set in
      let nrows' = nrows + List.length piece_string_list in
      let binmat = B.make nrows' ncols coords in
        Alg.solve binmat

    let show_solution piece_string_list board =
      match get_solution piece_string_list board with
        | None -> Printf.printf "No solution found.\n"
        | Some sol ->
            let pieces = make_piece_array piece_string_list board in
            let indices = IntSet.elements sol in
              if validate_solution board pieces indices
                then 
                  Printf.printf "%s" (string_of_solution board pieces indices)
                else 
                  Printf.printf "%s\n" "solution is not valid"
  end

module FS = FindSolutions(ImplBinaryMatrix)
open FS

(*** Utility functions ***)

let fatal_error msg =
  begin
    prerr_string msg;
    exit 1
  end

(* Get a list of pieces from a string of letters from 'a'-'k'. *)
let get_pieces_from_string pieces =
  let explode s = List.init (String.length s) (String.get s) in
  let get_piece_from_char = function
    | 'a' -> ('a', pa_s)
    | 'b' -> ('b', pb_s)
    | 'c' -> ('c', pc_s)
    | 'd' -> ('d', pd_s)
    | 'e' -> ('e', pe_s)
    | 'f' -> ('f', pf_s)
    | 'g' -> ('g', pg_s)
    | 'h' -> ('h', ph_s)
    | 'i' -> ('i', pi_s)
    | 'j' -> ('j', pj_s)
    | 'k' -> ('k', pk_s)
    | 'l' -> ('l', pl_s)
    | c   ->
      let msg = Printf.sprintf "ERROR: invalid piece name [%c]\n" c in
        fatal_error msg
  in
  if String.length pieces > 12 then
    fatal_error "ERROR: there are only 12 pieces maximum"
  else
    let chars = explode pieces in
    (* Check for duplicates and characters outside the range. *)
    let uniq_chars = List.sort_uniq compare chars in
    let bad_chars = List.filter (fun c -> c < 'a' || c > 'l') chars in
      match () with
        | _ when List.length uniq_chars < List.length chars ->
          let msg =
            Printf.sprintf 
              "ERROR: repeated characters in pieces string \"%s\"\n" pieces
          in
            fatal_error msg
        | _ when List.length bad_chars > 0 ->
          let msg =
            Printf.sprintf 
              "ERROR: bad characters in pieces string \"%s\"\n" pieces
          in
            fatal_error msg
        | _ -> List.map get_piece_from_char chars

let solve nrows ncols pieces =
  if nrows < 1 || ncols < 1 then
    fatal_error "ERROR: nrows and ncols must be > 0\n"
  else if nrows * ncols > 55 then
    fatal_error "ERROR: not enough pieces to fill a board that large!\n"
  else
    (* We need to seed the RNG twice to get any kind of real randomness
       in this program. *)
    let _ = (Random.self_init (); Random.init (Random.int 1000000)) in
    let board = { nrows; ncols } in
    let pieces' = get_pieces_from_string pieces in
      show_solution pieces' board

(*** Entry point ***)

let _ =
  let len = Array.length (Sys.argv) in
    if len < 3 || len > 4
      then
        begin
          Printf.fprintf stderr "usage: kanoodle nrows ncols [pstring]\n";
          Printf.fprintf stderr "  (where nrows/ncols > 0 and < 12)\n";
          Printf.fprintf stderr
            "  (where pstring is a string representing the allowed pieces)\n";
          exit 1
        end
      else
        try
          let nrows   = int_of_string (Sys.argv.(1)) in
          let ncols   = int_of_string (Sys.argv.(2)) in
          let pstring = 
            if len = 3 then "abcdefghijkl" else Sys.argv.(3) in
              solve nrows ncols pstring
        with (Failure msg) ->
          let msg' = Printf.sprintf "ERROR: %s\n" msg in
            fatal_error msg'

