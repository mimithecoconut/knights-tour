(*Student name: Shenyi Li*)
(*Email: sli5@caltech.edu*)

open Storage
open Board
open Utils

exception Solution_not_found

module type Searcher =
  sig
    val search : int -> int -> int -> int -> bool -> (Loc.t list) option
  end

module Make (S : Storage) : Searcher =
  struct
    module B = Board.Make(S)
    module P = Print(B)

    (* Helper functions go here. *)

      let rec min_loc min ls counts=
        match counts with
          | [] -> ls
          | (l, c) :: t ->
            if c > min then min_loc min ls t
            (* c = min add to answer list *)
            else if c = min then
                min_loc min (l :: ls) t
            (* count < min then reset answer list*)
            else 
               min_loc c [l] t
                
      (* Interface functions. *)
  
      let search nrows ncols start_row start_col print =
        let rec iter board =
          (*if board is solved *)
          if B.is_solved board then
            begin
              (*print board if print is true*)
              if print then P.print_board board false; 
              (*return list of locations placed*)
              Some (B.get_placed board)
            end
          else 
            (*start from last Knight location*)
            let last_loc = B.get_last board in
            (* Get all available locations *)
            let counts = B.get_loc_counts_from_loc board last_loc in
              begin 
                (*if no locations, then search fails*)
                if counts = [] then None 
                else
                  (*get list of minimum reachability counts*)
                  let (r, c) = B.get_dimensions board in 
                  let possible_loc = min_loc (r + c) [] counts in
                  (*pick one from list and place knight there*)
                  let rand_i = Random.int (List.length possible_loc) in
                  let new_loc = List.nth possible_loc rand_i in
                    iter (B.place board new_loc)
              end 
        in  
        (*iterate through new board with a knight placed at start location*)   
          iter (B.place (B.make nrows ncols) (start_row, start_col))
  end

