(* Tests for CS 4 final exam, Winter 2021, part A. *)

(*
To run this test script from the ocaml toplevel, do this:

$ ocaml
# #use "topfind";;  (* may not be necessary *)
# #require "oUnit";;
# open OUnit;;
# #load "binmat.cmo";;
# #use "tests_binmat.ml";;
*)

open OUnit2
open Binmat
open ImplBinaryMatrix


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

let make_tests =
  "make" >:: (fun _ ->
     expect_invalid_arg "make 1"
       (fun () -> make (-2) 3 [(0, 1); (1, 0); (1, 2)]);
     expect_invalid_arg "make 2"
       (fun () -> make 2 0 [(0, 1); (1, 0); (1, 2)]);
     expect_invalid_arg "make 3"
       (fun () -> make 2 3 [(0, 1); (1, -1); (1, 2)]);
     expect_invalid_arg "make 4"
       (fun () -> make 2 3 [(0, 1); (-1, 1); (1, 2)]);
     expect_invalid_arg "make 5"
       (fun () -> make 2 3 [(0, 1); (1, 0); (2, 2)]);
     expect_invalid_arg "make 6"
       (fun () -> make 2 3 [(0, 1); (1, 0); (1, 3)]);
     let m = make 2 3 [(0, 1); (1, 0); (1, 2)] in
       begin
         assert_bool "make 7" (nrows m = 2);
         assert_bool "make 8" (ncols m = 3);
         assert_bool "make 9"  (get m 0 0 = false);
         assert_bool "make 10" (get m 0 1 = true);
         assert_bool "make 11" (get m 0 2 = false);
         assert_bool "make 12" (get m 1 0 = true);
         assert_bool "make 13" (get m 1 1 = false);
         assert_bool "make 14" (get m 1 2 = true)
       end
  )

let rows_tests =
  "rows" >:: (fun _ ->
     let m = make 2 3 [(0, 1); (1, 0); (1, 2)] in
       assert_bool "rows 1" (equal_lists (rows m) [0; 1])
  )

let cols_tests =
  "cols" >:: (fun _ ->
     let m = make 2 3 [(0, 1); (1, 0); (1, 2)] in
     assert_bool "cols 1" (equal_lists (cols m) [0; 1; 2])
  )

let rows_for_col_tests =
  "rows_for_col" >:: (fun _ ->
     let m = make 4 5
       [(0, 0); (0, 4); (1, 1); (1, 2); (2, 0); (2, 2); (2, 3); (3, 2); (3, 4)]
     in
       begin
         expect_failure "rows_for_col 1" (fun () -> rows_for_col m (-1));
         assert_bool "rows_for_col 2"
           (equal_lists (rows_for_col m 0) [0; 2]);
         assert_bool "rows_for_col 3"
           (equal_lists (rows_for_col m 1) [1]);
         assert_bool "rows_for_col 4"
           (equal_lists (rows_for_col m 2) [1; 2; 3]);
         assert_bool "rows_for_col 5"
           (equal_lists (rows_for_col m 3) [2]);
         assert_bool "rows_for_col 6"
           (equal_lists (rows_for_col m 4) [0; 3]);
         expect_failure "rows_for_col 7" (fun () -> rows_for_col m 5)
       end
  )

let cols_for_row_tests =
  "cols_for_row" >:: (fun _ ->
     let m = make 4 5
       [(0, 0); (0, 4); (1, 1); (1, 2); (2, 0); (2, 2); (2, 3); (3, 2); (3, 4)]
     in
       begin
         expect_failure "cols_for_row 1" (fun () -> cols_for_row m (-1));
         assert_bool "cols_for_row 2"
           (equal_lists (cols_for_row m 0) [0; 4]);
         assert_bool "cols_for_row 3"
           (equal_lists (cols_for_row m 1) [1; 2]);
         assert_bool "cols_for_row 4"
           (equal_lists (cols_for_row m 2) [0; 2; 3]);
         assert_bool "cols_for_row 5"
           (equal_lists (cols_for_row m 3) [2; 4]);
         expect_failure "cols_for_row 6" (fun () -> cols_for_row m 4)
       end
  )

let delete_row_tests =
  "delete_row" >:: (fun _ ->
     let m = make 4 5
       [(0, 0); (0, 4); (1, 1); (1, 2); (2, 0); (2, 2); (2, 3); (3, 2); (3, 4)]
     in
       begin
         expect_failure "delete_row 1" (fun () -> delete_row m (-1));
         expect_failure "delete_row 2" (fun () -> delete_row m 4);

         let m' = delete_row m 0 in
           begin
             expect_failure "delete_row 3" (fun () -> delete_row m' 0);
             expect_failure "delete_row 4" (fun () -> cols_for_row m' (-1));
             expect_failure "delete_row 5" (fun () -> cols_for_row m' 4);
             expect_failure "delete_row 6" (fun () -> cols_for_row m' 0);
             assert_bool "delete_row 7"
               (equal_lists (cols_for_row m' 1) [1; 2]);
             assert_bool "delete_row 8"
               (equal_lists (cols_for_row m' 2) [0; 2; 3]);
             assert_bool "delete_row 9"
               (equal_lists (cols_for_row m' 3) [2; 4]);

             expect_failure "delete_row 10" (fun () -> rows_for_col m' (-1));
             expect_failure "delete_row 11" (fun () -> rows_for_col m' 5);
             assert_bool "delete_row 12"
               (equal_lists (rows_for_col m' 0) [2]);
             assert_bool "delete_row 13"
               (equal_lists (rows_for_col m' 1) [1]);
             assert_bool "delete_row 14"
               (equal_lists (rows_for_col m' 2) [1; 2; 3]);
             assert_bool "delete_row 15"
               (equal_lists (rows_for_col m' 3) [2]);
             assert_bool "delete_row 16"
               (equal_lists (rows_for_col m' 4) [3]);
           end;

         let m' = delete_row m 1 in
           begin
             expect_failure "delete_row 17" (fun () -> delete_row m' 1);
             expect_failure "delete_row 18" (fun () -> cols_for_row m' (-1));
             expect_failure "delete_row 19" (fun () -> cols_for_row m' 4);
             expect_failure "delete_row 20" (fun () -> cols_for_row m' 1);
             assert_bool "delete_row 21"
               (equal_lists (cols_for_row m' 0) [0; 4]);
             assert_bool "delete_row 22"
               (equal_lists (cols_for_row m' 2) [0; 2; 3]);
             assert_bool "delete_row 23"
               (equal_lists (cols_for_row m' 3) [2; 4]);

             expect_failure "delete_row 22" (fun () -> rows_for_col m' (-1));
             expect_failure "delete_row 23" (fun () -> rows_for_col m' 5);
             assert_bool "delete_row 24"
               (equal_lists (rows_for_col m' 0) [0; 2]);
             assert_bool "delete_row 25"
               (equal_lists (rows_for_col m' 1) []);
             assert_bool "delete_row 26"
               (equal_lists (rows_for_col m' 2) [2; 3]);
             assert_bool "delete_row 27"
               (equal_lists (rows_for_col m' 3) [2]);
             assert_bool "delete_row 28"
               (equal_lists (rows_for_col m' 4) [0; 3]);
           end;

         let m' = delete_row m 2 in
           begin
             expect_failure "delete_row 29" (fun () -> delete_row m' 2);
             expect_failure "delete_row 30" (fun () -> cols_for_row m' (-1));
             expect_failure "delete_row 31" (fun () -> cols_for_row m' 4);
             expect_failure "delete_row 32" (fun () -> cols_for_row m' 2);
             assert_bool "delete_row 33"
               (equal_lists (cols_for_row m' 0) [0; 4]);
             assert_bool "delete_row 34"
               (equal_lists (cols_for_row m' 1) [1; 2]);
             assert_bool "delete_row 35"
               (equal_lists (cols_for_row m' 3) [2; 4]);

             expect_failure "delete_row 36" (fun () -> rows_for_col m' (-1));
             expect_failure "delete_row 37" (fun () -> rows_for_col m' 5);
             assert_bool "delete_row 38"
               (equal_lists (rows_for_col m' 0) [0]);
             assert_bool "delete_row 39"
               (equal_lists (rows_for_col m' 1) [1]);
             assert_bool "delete_row 40"
               (equal_lists (rows_for_col m' 2) [1; 3]);
             assert_bool "delete_row 41"
               (equal_lists (rows_for_col m' 3) []);
             assert_bool "delete_row 42"
               (equal_lists (rows_for_col m' 4) [0; 3]);
           end;

         let m' = delete_row m 3 in
           begin
             expect_failure "delete_row 43" (fun () -> delete_row m' 3);
             expect_failure "delete_row 44" (fun () -> cols_for_row m' (-1));
             expect_failure "delete_row 45" (fun () -> cols_for_row m' 4);
             expect_failure "delete_row 46" (fun () -> cols_for_row m' 3);
             assert_bool "delete_row 47"
               (equal_lists (cols_for_row m' 0) [0; 4]);
             assert_bool "delete_row 48"
               (equal_lists (cols_for_row m' 1) [1; 2]);
             assert_bool "delete_row 49"
               (equal_lists (cols_for_row m' 2) [0; 2; 3]);

             expect_failure "delete_row 50" (fun () -> rows_for_col m' (-1));
             expect_failure "delete_row 51" (fun () -> rows_for_col m' 5);
             assert_bool "delete_row 52"
               (equal_lists (rows_for_col m' 0) [0; 2]);
             assert_bool "delete_row 53"
               (equal_lists (rows_for_col m' 1) [1]);
             assert_bool "delete_row 54"
               (equal_lists (rows_for_col m' 2) [1; 2]);
             assert_bool "delete_row 55"
               (equal_lists (rows_for_col m' 3) [2]);
             assert_bool "delete_row 56"
               (equal_lists (rows_for_col m' 4) [0]);
           end;
       end
  )

let delete_col_tests =
  "delete_col" >:: (fun _ ->
     let m = make 4 5
       [(0, 0); (0, 4); (1, 1); (1, 2); (2, 0); (2, 2); (2, 3); (3, 2); (3, 4)]
     in
       begin
         expect_failure "delete_col 1" (fun () -> delete_col m (-1));
         expect_failure "delete_col 2" (fun () -> delete_col m 5);

         let m' = delete_col m 0 in
           begin
             expect_failure "delete_col 3" (fun () -> delete_col m' 0);
             expect_failure "delete_col 4" (fun () -> rows_for_col m' (-1));
             expect_failure "delete_col 5" (fun () -> rows_for_col m' 5);
             expect_failure "delete_col 6" (fun () -> rows_for_col m' 0);
             assert_bool "delete_col 7"
               (equal_lists (rows_for_col m' 1) [1]);
             assert_bool "delete_col 8"
               (equal_lists (rows_for_col m' 2) [1; 2; 3]);
             assert_bool "delete_col 9"
               (equal_lists (rows_for_col m' 3) [2]);
             assert_bool "delete_col 10"
               (equal_lists (rows_for_col m' 4) [0; 3]);

             expect_failure "delete_col 11" (fun () -> cols_for_row m' (-1));
             expect_failure "delete_col 12" (fun () -> cols_for_row m' 4);
             assert_bool "delete_col 13"
               (equal_lists (cols_for_row m' 0) [4]);
             assert_bool "delete_col 14"
               (equal_lists (cols_for_row m' 1) [1; 2]);
             assert_bool "delete_col 15"
               (equal_lists (cols_for_row m' 2) [2; 3]);
             assert_bool "delete_col 16"
               (equal_lists (cols_for_row m' 3) [2; 4]);
           end;

         let m' = delete_col m 1 in
           begin
             expect_failure "delete_col 17" (fun () -> delete_col m' 1);
             expect_failure "delete_col 18" (fun () -> rows_for_col m' (-1));
             expect_failure "delete_col 19" (fun () -> rows_for_col m' 5);
             expect_failure "delete_col 20" (fun () -> rows_for_col m' 1);
             assert_bool "delete_col 21"
               (equal_lists (rows_for_col m' 0) [0; 2]);
             assert_bool "delete_col 22"
               (equal_lists (rows_for_col m' 2) [1; 2; 3]);
             assert_bool "delete_col 23"
               (equal_lists (rows_for_col m' 3) [2]);
             assert_bool "delete_col 24"
               (equal_lists (rows_for_col m' 4) [0; 3]);

             expect_failure "delete_col 25" (fun () -> cols_for_row m' (-1));
             expect_failure "delete_col 26" (fun () -> cols_for_row m' 4);
             assert_bool "delete_col 27"
               (equal_lists (cols_for_row m' 0) [0; 4]);
             assert_bool "delete_col 28"
               (equal_lists (cols_for_row m' 1) [2]);
             assert_bool "delete_col 29"
               (equal_lists (cols_for_row m' 2) [0; 2; 3]);
             assert_bool "delete_col 30"
               (equal_lists (cols_for_row m' 3) [2; 4]);
           end;

         let m' = delete_col m 2 in
           begin
             expect_failure "delete_col 31" (fun () -> delete_col m' 2);
             expect_failure "delete_col 32" (fun () -> rows_for_col m' (-1));
             expect_failure "delete_col 33" (fun () -> rows_for_col m' 5);
             expect_failure "delete_col 34" (fun () -> rows_for_col m' 2);
             assert_bool "delete_col 35"
               (equal_lists (rows_for_col m' 0) [0; 2]);
             assert_bool "delete_col 36"
               (equal_lists (rows_for_col m' 1) [1]);
             assert_bool "delete_col 37"
               (equal_lists (rows_for_col m' 3) [2]);
             assert_bool "delete_col 38"
               (equal_lists (rows_for_col m' 4) [0; 3]);

             expect_failure "delete_col 39" (fun () -> cols_for_row m' (-1));
             expect_failure "delete_col 40" (fun () -> cols_for_row m' 4);
             assert_bool "delete_col 41"
               (equal_lists (cols_for_row m' 0) [0; 4]);
             assert_bool "delete_col 42"
               (equal_lists (cols_for_row m' 1) [1]);
             assert_bool "delete_col 43"
               (equal_lists (cols_for_row m' 2) [0; 3]);
             assert_bool "delete_col 44"
               (equal_lists (cols_for_row m' 3) [4]);
           end;

         let m' = delete_col m 3 in
           begin
             expect_failure "delete_col 45" (fun () -> delete_col m' 3);
             expect_failure "delete_col 46" (fun () -> rows_for_col m' (-1));
             expect_failure "delete_col 47" (fun () -> rows_for_col m' 5);
             expect_failure "delete_col 48" (fun () -> rows_for_col m' 3);
             assert_bool "delete_col 49"
               (equal_lists (rows_for_col m' 0) [0; 2]);
             assert_bool "delete_col 50"
               (equal_lists (rows_for_col m' 1) [1]);
             assert_bool "delete_col 51"
               (equal_lists (rows_for_col m' 2) [1; 2; 3]);
             assert_bool "delete_col 52"
               (equal_lists (rows_for_col m' 4) [0; 3]);

             expect_failure "delete_col 53" (fun () -> cols_for_row m' (-1));
             expect_failure "delete_col 54" (fun () -> cols_for_row m' 4);
             assert_bool "delete_col 55"
               (equal_lists (cols_for_row m' 0) [0; 4]);
             assert_bool "delete_col 56"
               (equal_lists (cols_for_row m' 1) [1; 2]);
             assert_bool "delete_col 57"
               (equal_lists (cols_for_row m' 2) [0; 2]);
             assert_bool "delete_col 58"
               (equal_lists (cols_for_row m' 3) [2; 4]);
           end;

         let m' = delete_col m 4 in
           begin
             expect_failure "delete_col 59" (fun () -> delete_col m' 4);
             expect_failure "delete_col 60" (fun () -> rows_for_col m' (-1));
             expect_failure "delete_col 61" (fun () -> rows_for_col m' 5);
             expect_failure "delete_col 62" (fun () -> rows_for_col m' 4);
             assert_bool "delete_col 63"
               (equal_lists (rows_for_col m' 0) [0; 2]);
             assert_bool "delete_col 64"
               (equal_lists (rows_for_col m' 1) [1]);
             assert_bool "delete_col 65"
               (equal_lists (rows_for_col m' 2) [1; 2; 3]);
             assert_bool "delete_col 66"
               (equal_lists (rows_for_col m' 3) [2]);

             expect_failure "delete_col 67" (fun () -> cols_for_row m' (-1));
             expect_failure "delete_col 68" (fun () -> cols_for_row m' 4);
             assert_bool "delete_col 69"
               (equal_lists (cols_for_row m' 0) [0]);
             assert_bool "delete_col 70"
               (equal_lists (cols_for_row m' 1) [1; 2]);
             assert_bool "delete_col 71"
               (equal_lists (cols_for_row m' 2) [0; 2; 3]);
             assert_bool "delete_col 72"
               (equal_lists (cols_for_row m' 3) [2]);
           end;
       end
  )

let min_col_sum_index_tests =
  "min_col_sum_index" >:: (fun _ ->
     let m = make 4 5
       [(0, 0); (0, 4); (1, 1); (1, 2); (2, 0); (2, 2); (2, 3); (3, 2); (3, 4)]
     in
     let m'  = delete_row m 2 in
     let m'' = delete_col m 1 in
       begin
         assert_bool "min_col_sum_index 1"
           (min_col_sum_index m = 1 || min_col_sum_index m = 3);
         assert_bool "min_col_sum_index 2" (min_col_sum_index m' = 3);
         assert_bool "min_col_sum_index 3" (min_col_sum_index m'' = 3);
       end
  )

(* Comment out tests that you want to skip while developing the code. *)
let all_tests = "all_tests" >:::
[
  make_tests;
  rows_tests;
  cols_tests;
  rows_for_col_tests;
  cols_for_row_tests;
  delete_row_tests;
  delete_col_tests;
  min_col_sum_index_tests
]

let run_tests () =
  begin
    Printf.printf "\nRUNNING BINMAT TESTS...\n\n";
    run_test_tt_main all_tests;
  end

let _ = run_tests ()

