(* Tests for CS 4 final exam, Winter 2019, part B. *)

open Storage
open Board
open OUnit2
open Printf

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

(* Take the first n elements from a list. *)
let rec take n lst =
  match (n, lst) with
    | (0, _) -> []
    | (_, []) -> failwith "take: invalid input"
    | (_, h :: t) -> h :: take (n - 1) t

(* Convert a list of lists of integers to a string. *)
let string_of_list_of_lists lol =
  let string_of_list lst = 
    "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"
  in
    "[" ^ String.concat ";\n " (List.map string_of_list lol) ^ "]\n"

let print_list_of_lists lol =
  print_string (string_of_list_of_lists lol)

(* Compare two lists of (loc, int) pairs for equality.
 * Report an error if there is a discrepancy. *)
let compare_loc_int_lists label lil1 lil2 =
  let compare_loc_int i li1 li2 =
    if li1 <> li2 then
      let ((r1, c1), i1) = li1 in
      let ((r2, c2), i2) = li2 in
        begin
          printf "%s: index %d: different values: ((%d, %d), %d) vs ((%d, %d), %d)\n%!"
            label i r1 c1 i1 r2 c2 i2;
          false
        end
    else true
  in
  let len1 = List.length lil1 in
  let len2 = List.length lil2 in
    if len1 <> len2 then
      begin
        printf "%s: different numbers of items: %d vs %d\n%!"
          label len1 len2;
        false
      end
    else
      (* Sort lists so order doesn't matter. *)
      let slil1 = List.sort compare lil1 in
      let slil2 = List.sort compare lil2 in
      let rec iter i lil1 lil2 =
        match (lil1, lil2) with
          | ([], []) -> true  (* equal! *)
          | (h1 :: t1, h2 :: t2) ->
               if compare_loc_int i h1 h2 then
                 iter (i + 1) t1 t2
               else
                 false
          | _ -> failwith "this should never happen"
      in
        iter 0 slil1 slil2

(* Compare two lists of lists of integers for equality. 
 * Report an error if there is a discrepancy. *)
let compare_list_of_lists label lol1 lol2 =
  let compare_items r c i1 i2 =
    if i1 <> i2 then
      begin
        printf "%s: row %d: column %d: different values: %d vs %d\n%!"
          label r c i1 i2;
        false
      end
    else true
  in
  let compare_lists r lst1 lst2 =
    let len1 = List.length lst1 in
    let len2 = List.length lst2 in
      if len1 <> len2 then
        begin
          printf "%s: row %d: different numbers of columns: %d vs %d\n%!"
            label r len1 len2;
          false
        end
      else
        let rec iter_c c l1 l2 =
          match (l1, l2) with
            | ([], []) -> true  (* equal! *)
            | (h1 :: t1, h2 :: t2) -> 
                if compare_items r c h1 h2 then
                  iter_c (c + 1) t1 t2
                else
                  false
            | _ -> failwith "this should never happen"
        in
          iter_c 0 lst1 lst2
  in
  let len1 = List.length lol1 in
  let len2 = List.length lol2 in
    if len1 <> len2 then
      begin
        printf "%s: different numbers of rows: %d vs %d\n%!"
          label len1 len2;
        false
      end
    else
      let rec iter_r r l1 l2 =
        match (l1, l2) with
          | ([], []) -> true  (* equal! *)
          | (h1 :: t1, h2 :: t2) -> 
              if compare_lists r h1 h2 then
                iter_r (r + 1) t1 t2
              else
                false
          | _ -> failwith "this should never happen"
      in
        iter_r 0 lol1 lol2


module BoardTests (B: Board) =
  struct

    let get_all f board noneval =
      let (nrows, ncols) = B.get_dimensions board in
      let rec iter r c current_row prev_rows =
        match () with
          | _ when r = nrows -> 
            List.rev prev_rows
          | _ when c = ncols -> 
            iter (r + 1) 0 [] ((List.rev current_row) :: prev_rows)
          | _ -> 
            let curr = 
              match f board (r, c) with
                | None -> noneval
                | Some i -> i
            in
              iter r (c + 1) (curr :: current_row) prev_rows
      in
        iter 0 0 [] []

    (* Get all board indices as a list of lists. *)
    let get_all_index board = get_all B.get_index board 0

    (* Get all board reachables as a list of lists. *)
    let get_all_reachable board = get_all B.get_reachable board (-1)

    (* Get the initial reachables of an empty board with `nrows` rows
     * and `ncols` columns, as a list of lists. *)
    let get_init_reach nrows ncols =
      let board = B.make nrows ncols in
        get_all_reachable board

    (* Get loc counts from a loc as a list after n moves from a list
     * of placements. *)
    let get_loc_counts_from_loc_after_n board loc placements n =
      let b = 
        List.fold_left (fun b p -> B.place b p) board (take n placements)
      in
        B.get_loc_counts_from_loc b loc

    (* Get all board indices as a list of lists after n moves from a list
     * of placements. *)
    let place_all_get_indices board placements n =
      let b = 
        List.fold_left (fun b p -> B.place b p) board (take n placements)
      in
        get_all_index b

    (* Get all board reachables as a list of lists after n moves from a list
     * of placements. *)
    let place_all_get_reachable board placements n =
      let b = 
        List.fold_left (fun b p -> B.place b p) board (take n placements)
      in
        get_all_reachable b

    (* Like `place_all_get_indices`, but with the last move undone. *)
   let place_all_get_indices_undo board placements n =
      let b = 
        List.fold_left (fun b p -> B.place b p) board (take n placements)
      in
      let b' = B.undo b in
        get_all_index b'

    (* Like `place_all_get_reachables`, but with the last move undone. *)
    let place_all_get_reachable_undo board placements n =
      let b = 
        List.fold_left (fun b p -> B.place b p) board (take n placements)
      in
      let b' = B.undo b in
        get_all_reachable b'

  end

module BI = Board.Make(ImpStorage)
module BF = Board.Make(FunStorage)
module TI = BoardTests(BI)
module TF = BoardTests(BF)

(*** Useful testing data. ***)

(* Initial reachables of MxN boards, as lists of lists with one sublist
 * = one row. *)
let init_reach_1_1 = [[0]]
let init_reach_1_5 = [[0;0;0;0;0]]
let init_reach_3_4 = [[2;3;3;2];[2;2;2;2];[2;3;3;2]]
let init_reach_4_3 = [[2;2;2];[3;2;3];[3;2;3];[2;2;2]]
let init_reach_4_4 = [[2;3;3;2];[3;4;4;3];[3;4;4;3];[2;3;3;2]]
let init_reach_4_5 = [[2;3;4;3;2];[3;4;6;4;3];[3;4;6;4;3];[2;3;4;3;2]]
let init_reach_8_8 = 
  [
    [2;3;4;4;4;4;3;2];
    [3;4;6;6;6;6;4;3];
    [4;6;8;8;8;8;6;4];
    [4;6;8;8;8;8;6;4];
    [4;6;8;8;8;8;6;4];
    [4;6;8;8;8;8;6;4];
    [3;4;6;6;6;6;4;3];
    [2;3;4;4;4;4;3;2]
  ]

(* Moves (placements) required to make a knight's tour on a 3x4 board. *)
let moves_3_4 = 
  [(0,0);(1,2);(2,0);(0,1);(1,3);(2,1);(0,2);(2,3);(1,1);(0,3);(2,2);(1,0)]

(* Indices on a 3x4 board after N moves. *)
let index_3_4_0  = [[0;0;0;0]; [0;0;0;0]; [0;0;0;0]]
let index_3_4_1  = [[1;0;0;0]; [0;0;0;0]; [0;0;0;0]]
let index_3_4_2  = [[1;0;0;0]; [0;0;2;0]; [0;0;0;0]]
let index_3_4_3  = [[1;0;0;0]; [0;0;2;0]; [3;0;0;0]]
let index_3_4_4  = [[1;4;0;0]; [0;0;2;0]; [3;0;0;0]]
let index_3_4_5  = [[1;4;0;0]; [0;0;2;5]; [3;0;0;0]]
let index_3_4_6  = [[1;4;0;0]; [0;0;2;5]; [3;6;0;0]]
let index_3_4_7  = [[1;4;7;0]; [0;0;2;5]; [3;6;0;0]]
let index_3_4_8  = [[1;4;7;0]; [0;0;2;5]; [3;6;0;8]]
let index_3_4_9  = [[1;4;7;0]; [0;9;2;5]; [3;6;0;8]]
let index_3_4_10 = [[1;4;7;10];[0;9;2;5]; [3;6;0;8]]
let index_3_4_11 = [[1;4;7;10];[0;9;2;5]; [3;6;11;8]]
let index_3_4_12 = [[1;4;7;10];[12;9;2;5];[3;6;11;8]]

(* Reachables on a 3x4 board after N moves. *)
let reachable_3_4_0  = [[2;3;3;2];    [2;2;2;2];    [2;3;3;2]]
let reachable_3_4_1  = [[-1;3;3;2];   [2;2;1;2];    [2;2;3;2]]
let reachable_3_4_2  = [[-1;3;3;2];   [2;2;-1;2];   [1;2;3;2]]
let reachable_3_4_3  = [[-1;2;3;2];   [2;2;-1;2];   [-1;2;3;2]]
let reachable_3_4_4  = [[-1;-1;3;2];  [2;2;-1;1];   [-1;2;2;2]]
let reachable_3_4_5  = [[-1;-1;3;2];  [2;2;-1;-1];  [-1;1;2;2]]
let reachable_3_4_6  = [[-1;-1;2;2];  [2;2;-1;-1];  [-1;-1;2;2]]
let reachable_3_4_7  = [[-1;-1;-1;2]; [1;2;-1;-1];  [-1;-1;2;1]]
let reachable_3_4_8  = [[-1;-1;-1;2]; [1;1;-1;-1];  [-1;-1;2;-1]]
let reachable_3_4_9  = [[-1;-1;-1;1]; [1;-1;-1;-1]; [-1;-1;2;-1]]
let reachable_3_4_10 = [[-1;-1;-1;-1];[1;-1;-1;-1]; [-1;-1;1;-1]]
let reachable_3_4_11 = [[-1;-1;-1;-1];[0;-1;-1;-1]; [-1;-1;-1;-1]]
let reachable_3_4_12 = [[-1;-1;-1;-1];[-1;-1;-1;-1];[-1;-1;-1;-1]]

let loc_int_list_3_4_0_0_0 = [((1,2),2); ((2,1),3)]
let loc_int_list_3_4_0_0_1 = [((2,0),2); ((2,2),3); ((1,3),2)]

let loc_int_list_3_4_3_0_0 = [((2,1),2)]
let loc_int_list_3_4_3_1_2 = []
let loc_int_list_3_4_3_1_3 = [((0,1),2); ((2,1),2)]
let loc_int_list_3_4_3_2_0 = [((0,1),2)]


(*** The tests. ***)

let all_tests = "all_tests" >:::
[ 
  "make: invalid args" >:: (fun _ ->
     begin
       expect_invalid_arg "bad make 1" (fun _ -> BI.make (-1) (-1));
       expect_invalid_arg "bad make 2" (fun _ -> BF.make (-1) (-1));

       expect_invalid_arg "bad make 3" (fun _ -> BI.make (-1) 0);
       expect_invalid_arg "bad make 4" (fun _ -> BF.make (-1) 0);

       expect_invalid_arg "bad make 5" (fun _ -> BI.make 0 (-1));
       expect_invalid_arg "bad make 6" (fun _ -> BF.make 0 (-1));

       expect_invalid_arg "bad make 7" (fun _ -> BI.make 0 0);
       expect_invalid_arg "bad make 8" (fun _ -> BF.make 0 0);
     end
  );

  "make: init_reachable" >:: (fun _ ->
     begin
       assert_bool "init reach 1i"
         (compare_list_of_lists "init reach 1i" 
            (TI.get_init_reach 1 1) init_reach_1_1);

       assert_bool "init reach 2i"
         (compare_list_of_lists "init reach 2i" 
            (TI.get_init_reach 1 5) init_reach_1_5);

       assert_bool "init reach 3i"
         (compare_list_of_lists "init reach 3i" 
            (TI.get_init_reach 3 4) init_reach_3_4);

       assert_bool "init reach 4i"
         (compare_list_of_lists "init reach 4i" 
            (TI.get_init_reach 4 3) init_reach_4_3);

       assert_bool "init reach 5i"
         (compare_list_of_lists "init reach 5i" 
            (TI.get_init_reach 4 4) init_reach_4_4);

       assert_bool "init reach 6i"
         (compare_list_of_lists "init reach 6i" 
            (TI.get_init_reach 4 5) init_reach_4_5);

       assert_bool "init reach 7i"
         (compare_list_of_lists "init reach 7i" 
            (TI.get_init_reach 8 8) init_reach_8_8);


       assert_bool "init reach 1f"
         (compare_list_of_lists "init reach 1f" 
            (TF.get_init_reach 1 1) init_reach_1_1);

       assert_bool "init reach 2f"
         (compare_list_of_lists "init reach 2f" 
            (TF.get_init_reach 1 5) init_reach_1_5);

       assert_bool "init reach 3f"
         (compare_list_of_lists "init reach 3f" 
            (TF.get_init_reach 3 4) init_reach_3_4);

       assert_bool "init reach 4f"
         (compare_list_of_lists "init reach 4f" 
            (TF.get_init_reach 4 3) init_reach_4_3);

       assert_bool "init reach 5f"
         (compare_list_of_lists "init reach 5f" 
            (TF.get_init_reach 4 4) init_reach_4_4);

       assert_bool "init reach 6f"
         (compare_list_of_lists "init reach 6f" 
            (TF.get_init_reach 4 5) init_reach_4_5);

       assert_bool "init reach 7f"
         (compare_list_of_lists "init reach 7f" 
            (TF.get_init_reach 8 8) init_reach_8_8);
     end
  );

  "get_loc_counts_from_loc: bad input" >:: (fun _ ->
     begin
       expect_invalid_arg "bad get_loc_counts 1i" 
         (fun _ -> 
            let b = BI.make 3 4 in
              ignore (BI.get_loc_counts_from_loc b ((-1), (-1)))
         );

       expect_invalid_arg "bad get_loc_counts 2i" 
         (fun _ -> 
            let b = BI.make 3 4 in
              ignore (BI.get_loc_counts_from_loc b ((-1), 0))
         );

       expect_invalid_arg "bad get_loc_counts 3i" 
         (fun _ -> 
            let b = BI.make 3 4 in
              ignore (BI.get_loc_counts_from_loc b (0, (-1)))
         );

       expect_invalid_arg "bad get_loc_counts 1f" 
         (fun _ -> 
            let b = BF.make 3 4 in
              ignore (BF.get_loc_counts_from_loc b ((-1), (-1)))
         );

       expect_invalid_arg "bad get_loc_counts 2f" 
         (fun _ -> 
            let b = BF.make 3 4 in
              ignore (BF.get_loc_counts_from_loc b ((-1), 0))
         );

       expect_invalid_arg "bad get_loc_counts 3f" 
         (fun _ -> 
            let b = BF.make 3 4 in
              ignore (BF.get_loc_counts_from_loc b (0, (-1)))
         );
     end
  );

  "get_loc_counts_from_loc" >:: (fun _ ->
     begin
       assert_bool "get_loc_counts_from_loc 0i"
         (compare_loc_int_lists "get_loc_counts_from_loc 0i"
            (BI.get_loc_counts_from_loc (BI.make 3 4) (0, 0))
            loc_int_list_3_4_0_0_0);

       assert_bool "get_loc_counts_from_loc 1i"
         (compare_loc_int_lists "get_loc_counts_from_loc 1i"
            (BI.get_loc_counts_from_loc (BI.make 3 4) (0, 1))
            loc_int_list_3_4_0_0_1);

       assert_bool "get_loc_counts_from_loc 2i"
         (compare_loc_int_lists "get_loc_counts_from_loc 2i"
            (TI.get_loc_counts_from_loc_after_n (BI.make 3 4) (0, 0) moves_3_4 3)
            loc_int_list_3_4_3_0_0);

       assert_bool "get_loc_counts_from_loc 3i"
         (compare_loc_int_lists "get_loc_counts_from_loc 3i"
            (TI.get_loc_counts_from_loc_after_n (BI.make 3 4) (1, 2) moves_3_4 3)
            loc_int_list_3_4_3_1_2);

       assert_bool "get_loc_counts_from_loc 4i"
         (compare_loc_int_lists "get_loc_counts_from_loc 4i"
            (TI.get_loc_counts_from_loc_after_n (BI.make 3 4) (1, 3) moves_3_4 3)
            loc_int_list_3_4_3_1_3);

       assert_bool "get_loc_counts_from_loc 5i"
         (compare_loc_int_lists "get_loc_counts_from_loc 5i"
            (TI.get_loc_counts_from_loc_after_n (BI.make 3 4) (2, 0) moves_3_4 3)
            loc_int_list_3_4_3_2_0); 
     end
  );

  "place: bad input" >:: (fun _ ->
     begin
       expect_invalid_arg "bad place 1i" 
         (fun _ -> 
            let b = BI.make 3 4 in
              ignore (BI.place b ((-1), (-1)))
         );

       expect_invalid_arg "bad place 2i" 
         (fun _ -> 
            let b = BI.make 3 4 in
              ignore (BI.place b ((-1), 0))
         );

       expect_invalid_arg "bad place 3i" 
         (fun _ -> 
            let b = BI.make 3 4 in
              ignore (BI.place b (0, (-1)))
         );

       expect_invalid_arg "bad place 4i" 
         (fun _ -> 
            let b  = BI.make 3 4 in
            let b2 = BI.place b (0, 0) in
              ignore (BI.place b2 (0, 0))
         );

       expect_invalid_arg "bad place 5i" 
         (fun _ -> 
            let b  = BI.make 3 4 in
            let b2 = BI.place b (0, 0) in
              ignore (BI.place b2 (0, 1))
         );

       expect_invalid_arg "bad place 1f" 
         (fun _ -> 
            let b = BI.make 3 4 in
              ignore (BI.place b ((-1), (-1)))
         );

       expect_invalid_arg "bad place 2f" 
         (fun _ -> 
            let b = BI.make 3 4 in
              ignore (BI.place b ((-1), 0))
         );

       expect_invalid_arg "bad place 3f" 
         (fun _ -> 
            let b = BI.make 3 4 in
              ignore (BI.place b (0, (-1)))
         );

       expect_invalid_arg "bad place 4f" 
         (fun _ -> 
            let b  = BI.make 3 4 in
            let b2 = BI.place b (0, 0) in
              ignore (BI.place b2 (0, 0))
         );

       expect_invalid_arg "bad place 5f" 
         (fun _ -> 
            let b  = BI.make 3 4 in
            let b2 = BI.place b (0, 0) in
              ignore (BI.place b2 (0, 1))
         );

     end
  );

  "place: indices" >:: (fun _ ->
     begin
       assert_bool "place indices 0i"
         (compare_list_of_lists "place indices 0i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 0)
            index_3_4_0);

       assert_bool "place indices 1i"
         (compare_list_of_lists "place indices 1i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 1)
            index_3_4_1);

       assert_bool "place indices 2i"
         (compare_list_of_lists "place indices 2i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 2)
            index_3_4_2);

       assert_bool "place indices 3i"
         (compare_list_of_lists "place indices 3i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 3)
            index_3_4_3);

       assert_bool "place indices 4i"
         (compare_list_of_lists "place indices 4i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 4)
            index_3_4_4);

       assert_bool "place indices 5i"
         (compare_list_of_lists "place indices 5i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 5)
            index_3_4_5);

       assert_bool "place indices 6i"
         (compare_list_of_lists "place indices 6i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 6)
            index_3_4_6);

       assert_bool "place indices 7i"
         (compare_list_of_lists "place indices 7i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 7)
            index_3_4_7);

       assert_bool "place indices 8i"
         (compare_list_of_lists "place indices 8i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 8)
            index_3_4_8);

       assert_bool "place indices 9i"
         (compare_list_of_lists "place indices 9i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 9)
            index_3_4_9);

       assert_bool "place indices 10i"
         (compare_list_of_lists "place indices 10i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 10)
            index_3_4_10);

       assert_bool "place indices 11i"
         (compare_list_of_lists "place indices 11i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 11)
            index_3_4_11);

       assert_bool "place indices 12i"
         (compare_list_of_lists "place indices 12i"
            (TI.place_all_get_indices (BI.make 3 4) moves_3_4 12)
            index_3_4_12);


       assert_bool "place indices 0f"
         (compare_list_of_lists "place indices 0f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 0)
            index_3_4_0);

       assert_bool "place indices 1f"
         (compare_list_of_lists "place indices 1f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 1)
            index_3_4_1);

       assert_bool "place indices 2f"
         (compare_list_of_lists "place indices 2f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 2)
            index_3_4_2);

       assert_bool "place indices 3f"
         (compare_list_of_lists "place indices 3f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 3)
            index_3_4_3);

       assert_bool "place indices 4f"
         (compare_list_of_lists "place indices 4f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 4)
            index_3_4_4);

       assert_bool "place indices 5f"
         (compare_list_of_lists "place indices 5f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 5)
            index_3_4_5);

       assert_bool "place indices 6f"
         (compare_list_of_lists "place indices 6f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 6)
            index_3_4_6);

       assert_bool "place indices 7f"
         (compare_list_of_lists "place indices 7f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 7)
            index_3_4_7);

       assert_bool "place indices 8f"
         (compare_list_of_lists "place indices 8f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 8)
            index_3_4_8);

       assert_bool "place indices 9f"
         (compare_list_of_lists "place indices 9f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 9)
            index_3_4_9);

       assert_bool "place indices 10f"
         (compare_list_of_lists "place indices 10f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 10)
            index_3_4_10);

       assert_bool "place indices 11f"
         (compare_list_of_lists "place indices 11f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 11)
            index_3_4_11);

       assert_bool "place indices 12f"
         (compare_list_of_lists "place indices 12f"
            (TF.place_all_get_indices (BF.make 3 4) moves_3_4 12)
            index_3_4_12);

     end
  );

  "place: reachable" >:: (fun _ ->
     begin
       assert_bool "place reachable 0i"
         (compare_list_of_lists "place reachable 0i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 0)
            reachable_3_4_0);

       assert_bool "place reachable 1i"
         (compare_list_of_lists "place reachable 1i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 1)
            reachable_3_4_1);

       assert_bool "place reachable 2i"
         (compare_list_of_lists "place reachable 2i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 2)
            reachable_3_4_2);

       assert_bool "place reachable 3i"
         (compare_list_of_lists "place reachable 3i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 3)
            reachable_3_4_3);

       assert_bool "place reachable 4i"
         (compare_list_of_lists "place reachable 4i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 4)
            reachable_3_4_4);

       assert_bool "place reachable 5i"
         (compare_list_of_lists "place reachable 5i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 5)
            reachable_3_4_5);

       assert_bool "place reachable 6i"
         (compare_list_of_lists "place reachable 6i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 6)
            reachable_3_4_6);

       assert_bool "place reachable 7i"
         (compare_list_of_lists "place reachable 7i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 7)
            reachable_3_4_7);

       assert_bool "place reachable 8i"
         (compare_list_of_lists "place reachable 8i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 8)
            reachable_3_4_8);

       assert_bool "place reachable 9i"
         (compare_list_of_lists "place reachable 9i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 9)
            reachable_3_4_9);

       assert_bool "place reachable 10i"
         (compare_list_of_lists "place reachable 10i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 10)
            reachable_3_4_10);

       assert_bool "place reachable 11i"
         (compare_list_of_lists "place reachable 11i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 11)
            reachable_3_4_11);

       assert_bool "place reachable 12i"
         (compare_list_of_lists "place reachable 12i"
            (TI.place_all_get_reachable (BI.make 3 4) moves_3_4 12)
            reachable_3_4_12);


       assert_bool "place reachable 0f"
         (compare_list_of_lists "place reachable 0f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 0)
            reachable_3_4_0);

       assert_bool "place reachable 1f"
         (compare_list_of_lists "place reachable 1f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 1)
            reachable_3_4_1);

       assert_bool "place reachable 2f"
         (compare_list_of_lists "place reachable 2f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 2)
            reachable_3_4_2);

       assert_bool "place reachable 3f"
         (compare_list_of_lists "place reachable 3f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 3)
            reachable_3_4_3);

       assert_bool "place reachable 4f"
         (compare_list_of_lists "place reachable 4f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 4)
            reachable_3_4_4);

       assert_bool "place reachable 5f"
         (compare_list_of_lists "place reachable 5f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 5)
            reachable_3_4_5);

       assert_bool "place reachable 6f"
         (compare_list_of_lists "place reachable 6f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 6)
            reachable_3_4_6);

       assert_bool "place reachable 7f"
         (compare_list_of_lists "place reachable 7f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 7)
            reachable_3_4_7);

       assert_bool "place reachable 8f"
         (compare_list_of_lists "place reachable 8f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 8)
            reachable_3_4_8);

       assert_bool "place reachable 9f"
         (compare_list_of_lists "place reachable 9f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 9)
            reachable_3_4_9);

       assert_bool "place reachable 10f"
         (compare_list_of_lists "place reachable 10f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 10)
            reachable_3_4_10);

       assert_bool "place reachable 11f"
         (compare_list_of_lists "place reachable 11f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 11)
            reachable_3_4_11);

       assert_bool "place reachable 12f"
         (compare_list_of_lists "place reachable 12f"
            (TF.place_all_get_reachable (BF.make 3 4) moves_3_4 12)
            reachable_3_4_12);
     end
  );

  "undo: indices" >:: (fun _ ->
     begin
       assert_bool "undo indices 1i"
         (compare_list_of_lists "undo indices 1i"
            (TI.place_all_get_indices_undo (BI.make 3 4) moves_3_4 1)
            index_3_4_0);

       assert_bool "undo indices 2i"
         (compare_list_of_lists "undo indices 2i"
            (TI.place_all_get_indices_undo (BI.make 3 4) moves_3_4 2)
            index_3_4_1);

       assert_bool "undo indices 3i"
         (compare_list_of_lists "undo indices 3i"
            (TI.place_all_get_indices_undo (BI.make 3 4) moves_3_4 3)
            index_3_4_2);

       assert_bool "undo indices 4i"
         (compare_list_of_lists "undo indices 4i"
            (TI.place_all_get_indices_undo (BI.make 3 4) moves_3_4 4)
            index_3_4_3);

       assert_bool "undo indices 5i"
         (compare_list_of_lists "undo indices 5i"
            (TI.place_all_get_indices_undo (BI.make 3 4) moves_3_4 5)
            index_3_4_4);

       assert_bool "undo indices 6i"
         (compare_list_of_lists "undo indices 6i"
            (TI.place_all_get_indices_undo (BI.make 3 4) moves_3_4 6)
            index_3_4_5);

       assert_bool "undo indices 7i"
         (compare_list_of_lists "undo indices 7i"
            (TI.place_all_get_indices_undo (BI.make 3 4) moves_3_4 7)
            index_3_4_6);

       assert_bool "undo indices 8i"
         (compare_list_of_lists "undo indices 8i"
            (TI.place_all_get_indices_undo (BI.make 3 4) moves_3_4 8)
            index_3_4_7);

       assert_bool "undo indices 9i"
         (compare_list_of_lists "undo indices 9i"
            (TI.place_all_get_indices_undo (BI.make 3 4) moves_3_4 9)
            index_3_4_8);

       assert_bool "undo indices 10i"
         (compare_list_of_lists "undo indices 10i"
            (TI.place_all_get_indices_undo (BI.make 3 4) moves_3_4 10)
            index_3_4_9);

       assert_bool "undo indices 11i"
         (compare_list_of_lists "undo indices 11i"
            (TI.place_all_get_indices_undo (BI.make 3 4) moves_3_4 11)
            index_3_4_10);

       assert_bool "undo indices 12i"
         (compare_list_of_lists "undo indices 12i"
            (TI.place_all_get_indices_undo (BI.make 3 4) moves_3_4 12)
            index_3_4_11);


       assert_bool "undo indices 1f"
         (compare_list_of_lists "undo indices 1f"
            (TF.place_all_get_indices_undo (BF.make 3 4) moves_3_4 1)
            index_3_4_0);

       assert_bool "undo indices 2f"
         (compare_list_of_lists "undo indices 2f"
            (TF.place_all_get_indices_undo (BF.make 3 4) moves_3_4 2)
            index_3_4_1);

       assert_bool "undo indices 3f"
         (compare_list_of_lists "undo indices 3f"
            (TF.place_all_get_indices_undo (BF.make 3 4) moves_3_4 3)
            index_3_4_2);

       assert_bool "undo indices 4f"
         (compare_list_of_lists "undo indices 4f"
            (TF.place_all_get_indices_undo (BF.make 3 4) moves_3_4 4)
            index_3_4_3);

       assert_bool "undo indices 5f"
         (compare_list_of_lists "undo indices 5f"
            (TF.place_all_get_indices_undo (BF.make 3 4) moves_3_4 5)
            index_3_4_4);

       assert_bool "undo indices 6f"
         (compare_list_of_lists "undo indices 6f"
            (TF.place_all_get_indices_undo (BF.make 3 4) moves_3_4 6)
            index_3_4_5);

       assert_bool "undo indices 7f"
         (compare_list_of_lists "undo indices 7f"
            (TF.place_all_get_indices_undo (BF.make 3 4) moves_3_4 7)
            index_3_4_6);

       assert_bool "undo indices 8f"
         (compare_list_of_lists "undo indices 8f"
            (TF.place_all_get_indices_undo (BF.make 3 4) moves_3_4 8)
            index_3_4_7);

       assert_bool "undo indices 9f"
         (compare_list_of_lists "undo indices 9f"
            (TF.place_all_get_indices_undo (BF.make 3 4) moves_3_4 9)
            index_3_4_8);

       assert_bool "undo indices 10f"
         (compare_list_of_lists "undo indices 10f"
            (TF.place_all_get_indices_undo (BF.make 3 4) moves_3_4 10)
            index_3_4_9);

       assert_bool "undo indices 11f"
         (compare_list_of_lists "undo indices 11f"
            (TF.place_all_get_indices_undo (BF.make 3 4) moves_3_4 11)
            index_3_4_10);

       assert_bool "undo indices 12f"
         (compare_list_of_lists "undo indices 12f"
            (TF.place_all_get_indices_undo (BF.make 3 4) moves_3_4 12)
            index_3_4_11);

     end
  );

  "undo: reachable" >:: (fun _ ->
     begin
       assert_bool "undo reachable 1i"
         (compare_list_of_lists "undo reachable 1i"
            (TI.place_all_get_reachable_undo (BI.make 3 4) moves_3_4 1)
            reachable_3_4_0);

       assert_bool "undo reachable 2i"
         (compare_list_of_lists "undo reachable 2i"
            (TI.place_all_get_reachable_undo (BI.make 3 4) moves_3_4 2)
            reachable_3_4_1);

       assert_bool "undo reachable 3i"
         (compare_list_of_lists "undo reachable 3i"
            (TI.place_all_get_reachable_undo (BI.make 3 4) moves_3_4 3)
            reachable_3_4_2);

       assert_bool "undo reachable 4i"
         (compare_list_of_lists "undo reachable 4i"
            (TI.place_all_get_reachable_undo (BI.make 3 4) moves_3_4 4)
            reachable_3_4_3);

       assert_bool "undo reachable 5i"
         (compare_list_of_lists "undo reachable 5i"
            (TI.place_all_get_reachable_undo (BI.make 3 4) moves_3_4 5)
            reachable_3_4_4);

       assert_bool "undo reachable 6i"
         (compare_list_of_lists "undo reachable 6i"
            (TI.place_all_get_reachable_undo (BI.make 3 4) moves_3_4 6)
            reachable_3_4_5);

       assert_bool "undo reachable 7i"
         (compare_list_of_lists "undo reachable 7i"
            (TI.place_all_get_reachable_undo (BI.make 3 4) moves_3_4 7)
            reachable_3_4_6);

       assert_bool "undo reachable 8i"
         (compare_list_of_lists "undo reachable 8i"
            (TI.place_all_get_reachable_undo (BI.make 3 4) moves_3_4 8)
            reachable_3_4_7);

       assert_bool "undo reachable 9i"
         (compare_list_of_lists "undo reachable 9i"
            (TI.place_all_get_reachable_undo (BI.make 3 4) moves_3_4 9)
            reachable_3_4_8);

       assert_bool "undo reachable 10i"
         (compare_list_of_lists "undo reachable 10i"
            (TI.place_all_get_reachable_undo (BI.make 3 4) moves_3_4 10)
            reachable_3_4_9);

       assert_bool "undo reachable 11i"
         (compare_list_of_lists "undo reachable 11i"
            (TI.place_all_get_reachable_undo (BI.make 3 4) moves_3_4 11)
            reachable_3_4_10);

       assert_bool "undo reachable 12i"
         (compare_list_of_lists "undo reachable 12i"
            (TI.place_all_get_reachable_undo (BI.make 3 4) moves_3_4 12)
            reachable_3_4_11);


       assert_bool "undo reachable 1f"
         (compare_list_of_lists "undo reachable 1f"
            (TF.place_all_get_reachable_undo (BF.make 3 4) moves_3_4 1)
            reachable_3_4_0);

       assert_bool "undo reachable 2f"
         (compare_list_of_lists "undo reachable 2f"
            (TF.place_all_get_reachable_undo (BF.make 3 4) moves_3_4 2)
            reachable_3_4_1);

       assert_bool "undo reachable 3f"
         (compare_list_of_lists "undo reachable 3f"
            (TF.place_all_get_reachable_undo (BF.make 3 4) moves_3_4 3)
            reachable_3_4_2);

       assert_bool "undo reachable 4f"
         (compare_list_of_lists "undo reachable 4f"
            (TF.place_all_get_reachable_undo (BF.make 3 4) moves_3_4 4)
            reachable_3_4_3);

       assert_bool "undo reachable 5f"
         (compare_list_of_lists "undo reachable 5f"
            (TF.place_all_get_reachable_undo (BF.make 3 4) moves_3_4 5)
            reachable_3_4_4);

       assert_bool "undo reachable 6f"
         (compare_list_of_lists "undo reachable 6f"
            (TF.place_all_get_reachable_undo (BF.make 3 4) moves_3_4 6)
            reachable_3_4_5);

       assert_bool "undo reachable 7f"
         (compare_list_of_lists "undo reachable 7f"
            (TF.place_all_get_reachable_undo (BF.make 3 4) moves_3_4 7)
            reachable_3_4_6);

       assert_bool "undo reachable 8f"
         (compare_list_of_lists "undo reachable 8f"
            (TF.place_all_get_reachable_undo (BF.make 3 4) moves_3_4 8)
            reachable_3_4_7);

       assert_bool "undo reachable 9f"
         (compare_list_of_lists "undo reachable 9f"
            (TF.place_all_get_reachable_undo (BF.make 3 4) moves_3_4 9)
            reachable_3_4_8);

       assert_bool "undo reachable 10f"
         (compare_list_of_lists "undo reachable 10f"
            (TF.place_all_get_reachable_undo (BF.make 3 4) moves_3_4 10)
            reachable_3_4_9);

       assert_bool "undo reachable 11f"
         (compare_list_of_lists "undo reachable 11f"
            (TF.place_all_get_reachable_undo (BF.make 3 4) moves_3_4 11)
            reachable_3_4_10);

       assert_bool "undo reachable 12f"
         (compare_list_of_lists "undo reachable 12f"
            (TF.place_all_get_reachable_undo (BF.make 3 4) moves_3_4 12)
            reachable_3_4_11);

     end
  ); 

]

let run_tests () = 
  begin
    Printf.printf "\nRUNNING BOARD TESTS...\n\n";
    run_test_tt_main all_tests;
  end

let _ = run_tests ()

