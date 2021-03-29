(* Tests for CS 4 final exam, Winter 2021, part B. *)

(*
To run this test script from the ocaml toplevel, do this:

$ ocaml
# #use "topfind";;  (* may not be necessary *)
# #require "oUnit";;
# open OUnit;;
# #load "binmat.cmo";;
# #load "solver.cmo";;
# #use "tests_solver.ml";;
*)

open OUnit2
open Binmat
open Solver
open ImplBinaryMatrix

module Alg = AlgorithmX(ImplBinaryMatrix)

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

let all_tests = "all_tests" >:::
[ 
  "solve" >:: (fun _ ->
     let m = make 6 7 [(0, 0); (0, 3); (0, 6); 
                       (1, 0); (1, 3); 
                       (2, 3); (2, 4); (2, 6);
                       (3, 2); (3, 4); (3, 5);
                       (4, 1); (4, 2); (4, 5); (4, 6);
                       (5, 1); (5, 6)]
     in
     match Alg.solve m with
       | None -> assert_bool "solve 0" false  (* this shouldn't happen *)
       | Some sol ->
           let indices = IntSet.elements sol in
             assert_bool "solve 1" (equal_lists indices [1; 3; 5])
  );
]

let run_tests () = 
  begin
    Printf.printf "\nRUNNING SOLVER TESTS...\n\n";
    run_test_tt_main all_tests;
  end

let _ = run_tests ()

