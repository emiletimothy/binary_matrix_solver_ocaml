(* Tests for CS 4 final exam, Winter 2021, part C. *)

(*
To run this test script from the OCaml toplevel, do this:

$ ocaml
# #use "topfind";;  (* may not be necessary *)
# #require "oUnit";;
# open OUnit;;
# #load "kanoodle_types.cmo";;
# #load "kanoodle.cmo";;
# #use "tests_kanoodle.ml";;
*)

open OUnit2
open Kanoodle_pieces
open Kanoodle_types
open Kanoodle

(*** Utility functions. ***)

(* Expect an Invalid_argument exception. *)
let expect_invalid_arg msg thunk =
  assert_bool msg
    (try (thunk (); false)
     with Invalid_argument _ -> true)

(* Expect a Failure exception. *)
let expect_failure msg thunk =
  assert_bool msg
    (try (thunk (); false)
     with Failure _ -> true)

(* Test if two lists have the same elements in whatever order. *)
let equal_lists lst1 lst2 =
  let lst1' = List.sort compare lst1 in
  let lst2' = List.sort compare lst2 in
    lst1' = lst2'

(*** The tests. ***)

let translate_piece_tests = 
  "translate_piece" >:: (fun _ ->
    let p   = make_piece ('a', [(0, 0); (1, 0); (1, 1); (2, 1); (2, 2)]) in
    let p'  = translate_piece p 2 3 in
    let p'' = translate_piece p (-10) 200 in
      begin
        assert_bool "translate_piece 1" (p'.label = 'a' && p''.label = 'a');
        assert_bool "translate_piece 2" 
          (equal_lists (LocSet.elements p'.locs)
             [(2, 3); (3, 3); (3, 4); (4, 4); (4, 5)]);
        assert_bool "translate_piece 3" 
          (equal_lists (LocSet.elements p''.locs)
             [(-10, 200); (-9, 200); (-9, 201); (-8, 201); (-8, 202)])
      end
  )

let normalize_piece_tests =
  "normalize_piece" >:: (fun _ ->
    let p = make_piece ('a', [(10, 6); (11, 6); (11, 7); (12, 7); (12, 8)]) in
    let p' = normalize_piece p in
      begin
        assert_bool "normalize_piece 1" (p'.label = 'a');
        assert_bool "normalize_piece 2"
          (equal_lists (LocSet.elements p'.locs) 
             [(0, 0); (1, 0); (1, 1); (2, 1); (2, 2)])
      end;
    
    let p = 
      make_piece ('b', [(-5, -4); (-5, -3); (-5, -2); (-4, -4); (-3, -4)])
    in
    let p' = normalize_piece p in
      begin
        assert_bool "normalize_piece 3" (p'.label = 'b');
        assert_bool "normalize_piece 4" 
          (equal_lists (LocSet.elements p'.locs) 
             [(0, 0); (0, 1); (0, 2); (1, 0); (2, 0)])
      end;
  )

let reflect_piece_tests =
  "reflect_piece" >:: (fun _ ->
     let p = make_piece ('a', [(0, 0); (1, 0); (1, 1); (2, 1); (2, 2)]) in
     let p' = reflect_piece p in
     let q = make_piece ('c', [(10, 10); (11, 10); (12, 10); (12, 11); (12, 12)]) in
     let q' = reflect_piece q in
      begin
        assert_bool "reflect_piece 1" (p.label  = 'a');
        assert_bool "reflect_piece 2" (p'.label = 'a');
        assert_bool "reflect_piece 3" (q.label  = 'c');
        assert_bool "reflect_piece 4" (q'.label = 'c');
        assert_bool "reflect_piece 5" 
          (equal_lists (LocSet.elements p.locs) 
             [(0, 0); (1, 0); (1, 1); (2, 1); (2, 2)]);
        assert_bool "reflect_piece 6" 
          (equal_lists (LocSet.elements p'.locs) 
             [(0, 2); (1, 1); (1, 2); (2, 0); (2, 1)]);
        assert_bool "reflect_piece 7" 
          (equal_lists (LocSet.elements q.locs) 
             [(10, 10); (11, 10); (12, 10); (12, 11); (12, 12)]);
        assert_bool "reflect_piece 8" 
          (equal_lists (LocSet.elements q'.locs) 
             [(0, 2); (1, 2); (2, 0); (2, 1); (2, 2)]);
      end;
  )

let rotate_piece_tests =
  "rotate_piece" >:: (fun _ ->
     let p = make_piece ('a', [(0, 0); (1, 0); (1, 1); (2, 1); (2, 2)]) in
	 let p' = rotate_piece p in
	 let q = make_piece ('b', [(0, 1); (1, 0); (1, 1); (1, 2); (2, 1)]) in
	 let q' = rotate_piece q in
	 let r = make_piece ('c', [(0, 0); (1, 0); (2, 0); (2, 1); (2, 2)]) in
	 let r' = rotate_piece r in
	 let s = make_piece ('e', [(0, 1); (1, 0); (1, 1); (1, 2); (1, 3)]) in
	 let s' = rotate_piece s in
      begin
        assert_bool "rotate_piece 1" (p.label  = 'a');
        assert_bool "rotate_piece 2" (p'.label = 'a');
        assert_bool "rotate_piece 3" (q.label  = 'b');
        assert_bool "rotate_piece 4" (q'.label = 'b');
        assert_bool "rotate_piece 5" (r.label  = 'c');
        assert_bool "rotate_piece 6" (r'.label = 'c');
        assert_bool "rotate_piece 7" (s.label  = 'e');
        assert_bool "rotate_piece 8" (s'.label = 'e');
        assert_bool "rotate_piece 9" 
          (equal_lists (LocSet.elements p.locs) 
             [(0, 0); (1, 0); (1, 1); (2, 1); (2, 2)]);
        assert_bool "rotate_piece 10" 
          (equal_lists (LocSet.elements p'.locs) 
             [(0, 1); (0, 2); (1, 0); (1, 1); (2, 0)]);
        assert_bool "rotate_piece 11" 
          (equal_lists (LocSet.elements q.locs) 
             [(0, 1); (1, 0); (1, 1); (1, 2); (2, 1)]);
        assert_bool "rotate_piece 12" 
          (equal_lists (LocSet.elements q'.locs) 
             [(0, 1); (1, 0); (1, 1); (1, 2); (2, 1)]);
        assert_bool "rotate_piece 13" 
          (equal_lists (LocSet.elements r.locs) 
             [(0, 0); (1, 0); (2, 0); (2, 1); (2, 2)]);
        assert_bool "rotate_piece 14" 
          (equal_lists (LocSet.elements r'.locs) 
             [(0, 0); (0, 1); (0, 2); (1, 0); (2, 0)]);
        assert_bool "rotate_piece 15" 
          (equal_lists (LocSet.elements s.locs) 
             [(0, 1); (1, 0); (1, 1); (1, 2); (1, 3)]);
        assert_bool "rotate_piece 16" 
          (equal_lists (LocSet.elements s'.locs) 
             [(0, 0); (1, 0); (1, 1); (2, 0); (3, 0)]);
      end
  )

let translate_piece_on_board_tests =
  "translate_piece_on_board" >:: (fun _ ->
     let p    = make_piece ('a', [(0, 0); (1, 0); (1, 1); (2, 1); (2, 2)]) in

     let brd  = { nrows = 4; ncols = 4 } in
     let ps   = translate_piece_on_board brd p in
     let tps  = List.map (fun cs -> make_piece ('a', cs))
       [[(0, 0); (1, 0); (1, 1); (2, 1); (2, 2)];
        [(0, 1); (1, 1); (1, 2); (2, 2); (2, 3)];
        [(1, 0); (2, 0); (2, 1); (3, 1); (3, 2)];
        [(1, 1); (2, 1); (2, 2); (3, 2); (3, 3)]]
     in
     let brd2 = { nrows = 4; ncols = 5 } in
     let ps2  = translate_piece_on_board brd2 p in
     let tps2 = List.map (fun cs -> make_piece ('a', cs))
       [[(0, 0); (1, 0); (1, 1); (2, 1); (2, 2)];
        [(0, 1); (1, 1); (1, 2); (2, 2); (2, 3)];
        [(0, 2); (1, 2); (1, 3); (2, 3); (2, 4)];
        [(1, 0); (2, 0); (2, 1); (3, 1); (3, 2)];
        [(1, 1); (2, 1); (2, 2); (3, 2); (3, 3)];
        [(1, 2); (2, 2); (2, 3); (3, 3); (3, 4)]]
     in
     let brd3 = { nrows = 5; ncols = 4 } in
     let ps3  = translate_piece_on_board brd3 p in
     let tps3 = List.map (fun cs -> make_piece ('a', cs))
       [[(0, 0); (1, 0); (1, 1); (2, 1); (2, 2)];
        [(0, 1); (1, 1); (1, 2); (2, 2); (2, 3)];
        [(1, 0); (2, 0); (2, 1); (3, 1); (3, 2)];
        [(1, 1); (2, 1); (2, 2); (3, 2); (3, 3)];
        [(2, 0); (3, 0); (3, 1); (4, 1); (4, 2)];
        [(2, 1); (3, 1); (3, 2); (4, 2); (4, 3)]]
     in
     let brd4 = { nrows = 5; ncols = 5 } in
     let ps4  = translate_piece_on_board brd4 p in
     let tps4 = List.map (fun cs -> make_piece ('a', cs))
       [[(0, 0); (1, 0); (1, 1); (2, 1); (2, 2)];
        [(0, 1); (1, 1); (1, 2); (2, 2); (2, 3)];
        [(0, 2); (1, 2); (1, 3); (2, 3); (2, 4)];
        [(1, 0); (2, 0); (2, 1); (3, 1); (3, 2)];
        [(1, 1); (2, 1); (2, 2); (3, 2); (3, 3)];
        [(1, 2); (2, 2); (2, 3); (3, 3); (3, 4)];
        [(2, 0); (3, 0); (3, 1); (4, 1); (4, 2)];
        [(2, 1); (3, 1); (3, 2); (4, 2); (4, 3)];
        [(2, 2); (3, 2); (3, 3); (4, 3); (4, 4)]]
     in
     let brd5 = { nrows = 8; ncols = 8; } in
     let ps5  = translate_piece_on_board brd5 p in
     let tps5 = List.map (fun cs -> make_piece ('a', cs))
       [[(0, 0); (1, 0); (1, 1); (2, 1); (2, 2)]; 
        [(0, 1); (1, 1); (1, 2); (2, 2); (2, 3)]; 
        [(0, 2); (1, 2); (1, 3); (2, 3); (2, 4)]; 
        [(0, 3); (1, 3); (1, 4); (2, 4); (2, 5)]; 
        [(0, 4); (1, 4); (1, 5); (2, 5); (2, 6)]; 
        [(0, 5); (1, 5); (1, 6); (2, 6); (2, 7)]; 
        [(1, 0); (2, 0); (2, 1); (3, 1); (3, 2)]; 
        [(1, 1); (2, 1); (2, 2); (3, 2); (3, 3)]; 
        [(1, 2); (2, 2); (2, 3); (3, 3); (3, 4)]; 
        [(1, 3); (2, 3); (2, 4); (3, 4); (3, 5)]; 
        [(1, 4); (2, 4); (2, 5); (3, 5); (3, 6)]; 
        [(1, 5); (2, 5); (2, 6); (3, 6); (3, 7)]; 
        [(2, 0); (3, 0); (3, 1); (4, 1); (4, 2)]; 
        [(2, 1); (3, 1); (3, 2); (4, 2); (4, 3)]; 
        [(2, 2); (3, 2); (3, 3); (4, 3); (4, 4)]; 
        [(2, 3); (3, 3); (3, 4); (4, 4); (4, 5)]; 
        [(2, 4); (3, 4); (3, 5); (4, 5); (4, 6)]; 
        [(2, 5); (3, 5); (3, 6); (4, 6); (4, 7)]; 
        [(3, 0); (4, 0); (4, 1); (5, 1); (5, 2)]; 
        [(3, 1); (4, 1); (4, 2); (5, 2); (5, 3)]; 
        [(3, 2); (4, 2); (4, 3); (5, 3); (5, 4)]; 
        [(3, 3); (4, 3); (4, 4); (5, 4); (5, 5)]; 
        [(3, 4); (4, 4); (4, 5); (5, 5); (5, 6)]; 
        [(3, 5); (4, 5); (4, 6); (5, 6); (5, 7)]; 
        [(4, 0); (5, 0); (5, 1); (6, 1); (6, 2)]; 
        [(4, 1); (5, 1); (5, 2); (6, 2); (6, 3)]; 
        [(4, 2); (5, 2); (5, 3); (6, 3); (6, 4)]; 
        [(4, 3); (5, 3); (5, 4); (6, 4); (6, 5)]; 
        [(4, 4); (5, 4); (5, 5); (6, 5); (6, 6)]; 
        [(4, 5); (5, 5); (5, 6); (6, 6); (6, 7)]; 
        [(5, 0); (6, 0); (6, 1); (7, 1); (7, 2)]; 
        [(5, 1); (6, 1); (6, 2); (7, 2); (7, 3)]; 
        [(5, 2); (6, 2); (6, 3); (7, 3); (7, 4)]; 
        [(5, 3); (6, 3); (6, 4); (7, 4); (7, 5)]; 
        [(5, 4); (6, 4); (6, 5); (7, 5); (7, 6)]; 
        [(5, 5); (6, 5); (6, 6); (7, 6); (7, 7)]]
     in
       begin
         assert_bool "translate_piece_on_board 1a" 
           (PieceSet.cardinal ps = 4);
         List.iter 
           (fun p -> assert_bool "translate_piece_on_board 1b"
              (PieceSet.mem p ps)) tps;
         assert_bool "translate_piece_on_board 2a" 
           (PieceSet.cardinal ps2 = 6);
         List.iter 
           (fun p -> assert_bool "translate_piece_on_board 2b"
              (PieceSet.mem p ps2)) tps2;
         assert_bool "translate_piece_on_board 3a" 
           (PieceSet.cardinal ps3 = 6);
         List.iter 
           (fun p -> assert_bool "translate_piece_on_board 3b"
              (PieceSet.mem p ps3)) tps3;
         assert_bool "translate_piece_on_board 4a" 
           (PieceSet.cardinal ps4 = 9);
         List.iter 
           (fun p -> assert_bool "translate_piece_on_board 4b"
              (PieceSet.mem p ps4)) tps4;
         assert_bool "translate_piece_on_board 5a" 
           (PieceSet.cardinal ps5 = 36);
         List.iter 
           (fun p -> assert_bool "translate_piece_on_board 5b"
              (PieceSet.mem p ps5)) tps5;
       end
  )

let make_piece_array_tests =
  "make_piece_array" >:: (fun _ ->
     let piece_string_list = Kanoodle_pieces.all_pieces in
     let brd = { nrows = 5; ncols = 11; } in
     let arr = make_piece_array piece_string_list brd in
       assert_bool "make_piece_array 1" (Array.length arr = 1789)
  )

let make_constraints_array_tests =
  "make_constraints_array" >:: (fun _ ->
     let piece_string_list = Kanoodle_pieces.all_pieces in
     let brd = { nrows = 4; ncols = 5 } in
     let ca = make_constraints_array piece_string_list brd in
     let ca_expected_as_list =
       [Loc (3, 4); Loc (3, 3); Loc (3, 2); Loc (3, 1); Loc (3, 0); Loc (2, 4);
        Loc (2, 3); Loc (2, 2); Loc (2, 1); Loc (2, 0); Loc (1, 4); Loc (1, 3);
        Loc (1, 2); Loc (1, 1); Loc (1, 0); Loc (0, 4); Loc (0, 3); Loc (0, 2);
        Loc (0, 1); Loc (0, 0); Label 'a'; Label 'b'; Label 'c'; Label 'd';
        Label 'e'; Label 'f'; Label 'g'; Label 'h'; Label 'i'; Label 'j'; Label 'k';
        Label 'l']
     in
       assert_bool "make_constraints_array 1"
         (equal_lists (Array.to_list ca) ca_expected_as_list)
  )

let make_constraints_map_tests =
  "make_constraints_map" >:: (fun _ ->
     let piece_string_list = Kanoodle_pieces.all_pieces in
     let brd = { nrows = 5; ncols = 4 } in
     let cmap = make_constraints_map piece_string_list brd in
     let cmap_as_list = KconstraintMap.bindings cmap in
     let cmap_expected_as_list =
       [(Loc (0, 0), 19); (Loc (0, 1), 18); (Loc (0, 2), 17); (Loc (0, 3), 16);
        (Loc (1, 0), 15); (Loc (1, 1), 14); (Loc (1, 2), 13); (Loc (1, 3), 12);
        (Loc (2, 0), 11); (Loc (2, 1), 10); (Loc (2, 2), 9); (Loc (2, 3), 8);
        (Loc (3, 0), 7); (Loc (3, 1), 6); (Loc (3, 2), 5); (Loc (3, 3), 4);
        (Loc (4, 0), 3); (Loc (4, 1), 2); (Loc (4, 2), 1); (Loc (4, 3), 0);
        (Label 'a', 20); (Label 'b', 21); (Label 'c', 22); (Label 'd', 23);
        (Label 'e', 24); (Label 'f', 25); (Label 'g', 26); (Label 'h', 27);
        (Label 'i', 28); (Label 'j', 29); (Label 'k', 30); (Label 'l', 31)]
     in
       assert_bool "make_constraints_map 1"
         (equal_lists (cmap_as_list) cmap_expected_as_list)
  )

let make_binary_matrix_base_locs_tests =
  "make_binary_matrix_base_locs" >:: (fun _ ->
     let board = { nrows = 3; ncols = 5; } in
     let pi_s = "\nX..\nXXX\n" in
     let pj_s = "\nXXXX\n" in
     let pk_s = "\nXX\nXX\n" in
     let pieces = [('i', pi_s); ('j', pj_s); ('k', pk_s)] in
     let pa = make_piece_array pieces board in        (* not actually needed here *)
     let ca = make_constraints_array pieces board in  (* not actually needed here *)
     let locs = make_binary_matrix_base_locs pieces board in
       begin
         assert_bool "make_binary_matrix_base_locs 1"
           (Array.length pa = 54);
         assert_bool "make_binary_matrix_base_locs 2"
           (Array.length ca = 18);
         assert_bool "make_binary_matrix_base_locs 3"
           (LocSet.cardinal locs = 270);
       end
  )

(* Comment out tests that you want to skip while developing the code. *)
let all_tests = "all_tests" >:::
[ 
  translate_piece_tests;
  normalize_piece_tests;
  reflect_piece_tests;
  rotate_piece_tests;
  translate_piece_on_board_tests;
  make_piece_array_tests;
  make_constraints_array_tests;
  make_constraints_map_tests;
  make_binary_matrix_base_locs_tests
]

let run_tests () = 
  begin
    Printf.printf "\nRUNNING KANOODLE TESTS...\n\n";
    run_test_tt_main all_tests;
  end

let _ = run_tests ()

