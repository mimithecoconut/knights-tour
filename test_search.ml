(* Tests for CS 4 final exam, Winter 2019, part C. *)

open Storage
open Utils
open Board
open Search
open OUnit2

module Test(S: Searcher) =
  struct
    (* Run a search at most n times. If a valid solution is found, return
     * true. If an invalid solution is found, return false.
     * If no solution is found, return false. *)
    let run_search nrows ncols start_row start_col n = 
      let rec iter i =
        if i = n then
          false
        else
          match S.search nrows ncols start_row start_col false with
            | None -> iter (i + 1)
            | Some locs -> check_solution nrows ncols locs
      in
        iter 0
  end

module SI = Search.Make(ImpStorage)
module SF = Search.Make(FunStorage)

module TI = Test(SI)
module TF = Test(SF)

(*** The tests. ***)

let all_tests = "all_tests" >:::
[ 
  "search" >:: (fun _ ->
      begin
        (* 3x3 board has no solution. *)
        assert_bool "3x3 (0, 0), imp" (not (TI.run_search 3 3 0 0 10));
        assert_bool "3x3 (0, 0), fun" (not (TF.run_search 3 3 0 0 10));

        assert_bool "3x3 (1, 1), imp" (not (TI.run_search 3 3 1 1 10));
        assert_bool "3x3 (1, 1), fun" (not (TF.run_search 3 3 1 1 10));

        (* 4x4 board has no solution. *)

        assert_bool "4x4 (0, 0), imp" (not (TI.run_search 4 4 0 0 10));
        assert_bool "4x4 (0, 0), fun" (not (TF.run_search 4 4 0 0 10));

        assert_bool "4x4 (1, 1), imp" (not (TI.run_search 4 4 1 1 10));
        assert_bool "4x4 (1, 1), fun" (not (TF.run_search 4 4 1 1 10));

        (* 5x5 board has a solution starting from (0,0) but not from (0, 1). *)

        assert_bool "5x5 (0, 0), imp" (TI.run_search 5 5 0 0 10);
        assert_bool "5x5 (0, 0), fun" (TF.run_search 5 5 0 0 10);

        assert_bool "5x5 (0, 1), imp" (not (TI.run_search 5 5 0 1 10));
        assert_bool "5x5 (0, 1), fun" (not (TF.run_search 5 5 0 1 10));

        (* 8x8 board has many solutions. *)

        assert_bool "8x8 (0, 0), imp" (TI.run_search 8 8 0 0 10);
        assert_bool "8x8 (0, 0), fun" (TF.run_search 8 8 0 0 10);

        assert_bool "8x8 (1, 0), imp" (TI.run_search 8 8 1 0 10);
        assert_bool "8x8 (1, 0), fun" (TF.run_search 8 8 1 0 10);

        assert_bool "8x8 (2, 3), imp" (TI.run_search 8 8 2 3 10);
        assert_bool "8x8 (2, 3), fun" (TF.run_search 8 8 2 3 10);

        (* 10x10 board has many solutions. *)

        assert_bool "10x10 (0, 0), imp" (TI.run_search 10 10 0 0 10);
        assert_bool "10x10 (0, 0), fun" (TF.run_search 10 10 0 0 10);

        assert_bool "10x10 (1, 0), imp" (TI.run_search 10 10 1 0 10);
        assert_bool "10x10 (1, 0), fun" (TF.run_search 10 10 1 0 10);

        assert_bool "10x10 (2, 3), imp" (TI.run_search 10 10 2 3 10);
        assert_bool "10x10 (2, 3), fun" (TF.run_search 10 10 2 3 10);

        (* Larger boards starting from (0, 0).  All solvable. *)

        assert_bool "15x15 (0, 0), imp" (TI.run_search 15 15 0 0 15);
        assert_bool "15x15 (0, 0), fun" (TF.run_search 15 15 0 0 15);

        assert_bool "20x20 (0, 0), imp" (TI.run_search 20 20 0 0 20);
        assert_bool "20x20 (0, 0), fun" (TF.run_search 20 20 0 0 20);

        assert_bool "30x30 (0, 0), imp" (TI.run_search 30 30 0 0 30);
        assert_bool "30x30 (0, 0), fun" (TF.run_search 30 30 0 0 30);

        assert_bool "50x50 (0, 0), imp" (TI.run_search 50 50 0 0 50);
        assert_bool "50x50 (0, 0), fun" (TF.run_search 50 50 0 0 50);
        assert_bool "100x100 (0, 0), imp" (TI.run_search 100 100 0 0 100);
        assert_bool "100x100 (0, 0), fun" (TF.run_search 100 100 0 0 100);
      end
  );

]

let run_tests () = 
  begin
    Printf.printf "\nRUNNING SEARCH TESTS...\n\n";
    Random.self_init ();
    run_test_tt_main all_tests;
  end

let _ = run_tests ()

