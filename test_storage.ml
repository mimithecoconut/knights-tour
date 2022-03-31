(* Tests for CS 4 final exam, Winter 2019, part A. *)

open Storage
open OUnit2

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

module Test(S: Storage) =
  struct
    (* Test if a storage grid is completely unoccupied. *)
    let all_unoccupied nrows ncols =
      let grid = S.make nrows ncols in
        try
          begin
            for r = 0 to nrows - 1 do
              for c = 0 to ncols - 1 do
                if S.get grid (r, c) <> None then
                  raise Exit
              done
            done;
            true
          end
        with Exit -> false

    (* Test if doing `get` on a location that was `set` returns the value set. 
     * Also check that no other location was affected. *)
    let get_set nrows ncols =
      let row   = Random.int nrows in
      let col   = Random.int ncols in
      let value = Random.int 10 in
      let grid  = S.set (S.make nrows ncols) (row, col) value in
        try
          begin
            for r = 0 to nrows - 1 do
              for c = 0 to ncols - 1 do
                if r = row && c = col then
                  (if S.get grid (r, c) <> Some value then raise Exit)
                else 
                  (if S.get grid (r, c) <> None then raise Exit)
              done
            done;
            true
          end
        with Exit -> false 

    (* Try to set a location off the board. *)
    let bad_set_1 nrows ncols =
      let row   = -1 in
      let col   = Random.int ncols in
      let value = Random.int 10 in
        ignore (S.set (S.make nrows ncols) (row, col) value)

    (* Try to set a location off the board. *)
    let bad_set_2 nrows ncols =
      let row   = Random.int nrows in
      let col   = -1 in
      let value = Random.int 10 in
        ignore (S.set (S.make nrows ncols) (row, col) value)

    (* Try to set a location off the board. *)
    let bad_set_3 nrows ncols =
      let row   = nrows in
      let col   = Random.int ncols in
      let value = Random.int 10 in
        ignore (S.set (S.make nrows ncols) (row, col) value)

    (* Try to set a location off the board. *)
    let bad_set_4 nrows ncols =
      let row   = Random.int nrows in
      let col   = ncols in
      let value = Random.int 10 in
        ignore (S.set (S.make nrows ncols) (row, col) value)

    (* Try to set a negative number. *)
    let bad_set_5 nrows ncols =
      let row   = Random.int nrows in
      let col   = Random.int ncols in
      let value = -1 in
        ignore (S.set (S.make nrows ncols) (row, col) value)

    (* Test if doing `remove` on a location that was `set` clears the value.
     * Also check that no other location was affected. *)
    let remove_has_loc nrows ncols =
      let row   = Random.int nrows in
      let col   = Random.int ncols in
      let value = Random.int 10 in
      let grid  = S.set (S.make nrows ncols) (row, col) value in
      let grid' = S.remove grid (row, col) in
        try
          begin
            for r = 0 to nrows - 1 do
              for c = 0 to ncols - 1 do
                if r = row && c = col then
                  begin
                    if S.get grid' (r, c) <> None then raise Exit;
                    if S.has_loc grid' (r, c) then raise Exit;
                  end
                else 
                  (if S.get grid (r, c) <> None then raise Exit)
              done
            done;
            true
          end
        with Exit -> false
  end

module SI = ImpStorage
module SF = FunStorage

module TI = Test(ImpStorage)
module TF = Test(FunStorage)


(*** The tests. ***)

let all_tests = "all_tests" >:::
[ 
  "make" >:: (fun _ ->
     begin
       (* Invalid invocations of `make`. *)
       expect_invalid_arg "make 1" (fun _ -> ignore (SI.make (-1) (-1)));
       expect_invalid_arg "make 2" (fun _ -> ignore (SF.make (-1) (-1)));
       expect_invalid_arg "make 3" (fun _ -> ignore (SI.make (-1) (0)));
       expect_invalid_arg "make 4" (fun _ -> ignore (SF.make (-1) (0)));
       expect_invalid_arg "make 5" (fun _ -> ignore (SI.make 0 (-1)));
       expect_invalid_arg "make 6" (fun _ -> ignore (SF.make 0 (-1)));
       expect_invalid_arg "make 7" (fun _ -> ignore (SI.make 0 0));
       expect_invalid_arg "make 8" (fun _ -> ignore (SF.make 0 0));

       (* Valid invocations of `make`.  
        * All valid locations start out unoccupied. *)
       assert_bool "make 9"  (TI.all_unoccupied 3 4);
       assert_bool "make 10" (TF.all_unoccupied 3 4);
       assert_bool "make 11" (TI.all_unoccupied 8 8);
       assert_bool "make 12" (TF.all_unoccupied 8 8);
     end
  );

  "set/get" >:: (fun _ ->
     begin
       assert_bool "set/get 1"  (TI.get_set 3 4);
       assert_bool "set/get 2"  (TF.get_set 3 4);
       assert_bool "set/get 3"  (TI.get_set 5 5);
       assert_bool "set/get 4"  (TF.get_set 7 8);
       assert_bool "set/get 5"  (TI.get_set 7 8);
       assert_bool "set/get 6"  (TF.get_set 5 5);
     end
  );

  "bad set" >:: (fun _ ->
     begin
       expect_invalid_arg "bad set 1 I" 
         (fun _ -> TI.bad_set_1 3 4);
       expect_invalid_arg "bad set 2 I" 
         (fun _ -> TI.bad_set_2 3 4);
       expect_invalid_arg "bad set 3 I" 
         (fun _ -> TI.bad_set_3 3 4);
       expect_invalid_arg "bad set 4 I" 
         (fun _ -> TI.bad_set_4 3 4);
       expect_invalid_arg "bad set 5 I" 
         (fun _ -> TI.bad_set_5 3 4);

       expect_invalid_arg "bad set 1 F" 
         (fun _ -> TF.bad_set_1 3 4);
       expect_invalid_arg "bad set 2 F" 
         (fun _ -> TF.bad_set_2 3 4);
       expect_invalid_arg "bad set 3 F" 
         (fun _ -> TF.bad_set_3 3 4);
       expect_invalid_arg "bad set 4 F" 
         (fun _ -> TF.bad_set_4 3 4);
       expect_invalid_arg "bad set 5 F" 
         (fun _ -> TF.bad_set_5 3 4);
     end
  );

  "remove/has_loc" >:: (fun _ ->
     begin
       assert_bool "remove/has_loc 1 I"  (TI.remove_has_loc 3 4);
       assert_bool "remove/has_loc 2 I"  (TI.remove_has_loc 4 5);
       assert_bool "remove/has_loc 3 I"  (TI.remove_has_loc 8 8);

       assert_bool "remove/has_loc 1 F"  (TF.remove_has_loc 4 5);
       assert_bool "remove/has_loc 2 F"  (TF.remove_has_loc 4 5);
       assert_bool "remove/has_loc 3 F"  (TF.remove_has_loc 8 8);
     end 
  );
]

let run_tests () = 
  begin
    Random.self_init ();
    Printf.printf "\nRUNNING STORAGE TESTS...\n\n";
    run_test_tt_main all_tests;
  end

let _ = run_tests ()

