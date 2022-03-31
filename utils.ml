open Board
open Storage
open Printf

module Print (B : Board) =
  struct
    let print_board board print_all =
      let (nrows, ncols) = B.get_dimensions board in
      let max_index = nrows * ncols in
      let width = 
        match () with
          | _ when max_index < 10 -> 1
          | _ when max_index < 100 -> 2
          | _ when max_index < 1000 -> 3
          | _ when max_index < 10000 -> 4
          | _ -> 0  (* why bother? *)
      in
      let print_header () = 
        begin
          printf "%*s   " width "";
          for c = 0 to ncols - 1 do
            printf " %*d " width c
          done;
          printf "\n%!"
        end
      in
      let print_line () =
        let segment = String.make (width + 2) '-' in
        begin
          printf "%*s +-" width "";
          for c = 0 to ncols - 1 do
            printf "%s" segment
          done;
          printf "-+\n%!"
        end
      in
      let print_grid getter width =
        begin
          print_header ();
          print_line ();
          for r = 0 to nrows - 1 do
            begin
              printf "%*d | " width r;
              for c = 0 to ncols - 1 do
                match getter board (r, c) with
                  | None   -> printf " %*s " width "."
                  | Some i -> printf " %*d " width i
              done;
              printf " |\n%!"
            end
          done;
          print_line ();
          printf "\n%!"
        end
      in
        begin
          printf "\n%!";
          print_grid B.get_index width;
          if print_all then
            begin
              print_grid B.get_reachable width;
              printf "\n%!"
            end
        end
  end

module LocSet = Set.Make(Loc)

let is_knights_move (row1, col1) (row2, col2) =
  let row_diff = abs (row1 - row2) in
  let col_diff = abs (col1 - col2) in
  let diff = (row_diff, col_diff) in
    (diff = (1, 2)) || (diff = (2, 1))

let check_knights locs =
  let rec iter h t =
    match t with
      | [] -> true
      | h' :: t' ->
        if is_knights_move h h' then
          iter h' t'
        else
          let (r1, c1) = h in
          let (r2, c2) = h in
            begin
              fprintf stderr 
                "check_solution: invalid knight's move: (%d, %d) to (%d, %d)\n%!"
                r1 c1 r2 c2;
              false
            end
  in
  match locs with
    | [] -> true
    | h :: t -> iter h t

let check_solution nrows ncols locs =
  let size = nrows * ncols in
    if List.length locs <> size then
      begin
        fprintf stderr 
          "check_solution: error: not enough locations; expecting %d, got %d\n%!"
          size (List.length locs);
        false
      end
    else
      let s = LocSet.of_list locs in
        if LocSet.cardinal s <> size then
          begin
            fprintf stderr 
              "check_solution: error: repeated locations\n%!";
            false
          end
        else
          (* OK, we have enough unique elements.
           * Check that they are all inside the board. *)
          if not (List.for_all 
                    (fun (r, c) -> 
                          r >= 0 
                       && c >= 0 
                       && r < nrows 
                       && c < ncols)
                    locs) then
            begin
              fprintf stderr 
                "check_solution: error: some locations are off the board\n%!";
              false
            end
          else
            (* Check that each one is a knight's move away 
             * from the previous one. *)
            check_knights locs

