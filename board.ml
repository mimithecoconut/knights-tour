(*Student name: Shenyi Li*)
(*Email: sli5@caltech.edu*)

open Storage

(*
 * Helper functions.
 *)

(* Return `true` if a loc is valid on a board of size
 * `nrows` rows by `ncols` columns. *)
let ok_loc nrows ncols (row, col) =
  row >= 0 && row < nrows && col >= 0 && col < ncols

(* Raise an `Invalid_argument` exception due to a bad location.
 * `name` is the name of the function, and `cause` is the
 * reason for the argument being bad. *)
let bad_loc name cause (row, col) =
  let msg = 
    Printf.sprintf "%s: %s;  row = %d col = %d%!" 
      name cause row col
  in
    invalid_arg msg

(*
 * The board module type and module.  
 * It represents the state of the knight's tour solution.
 *)

module type Board =
  sig
    type loc = Loc.t
    type t

    val make                    : int -> int -> t
    val get_dimensions          : t -> int * int
    val get_last                : t -> loc
    val get_index               : t -> loc -> int option
    val get_reachable           : t -> loc -> int option
    val get_loc_counts_from_loc : t -> loc -> (loc * int) list
    val place                   : t -> loc -> t
    val undo                    : t -> t
    val is_solved               : t -> bool
    val get_placed              : t -> loc list
  end

module Make (S: Storage) : Board =
  struct
    type loc = Loc.t

    type t = 
      {
        nrows      : int;
        ncols      : int;
        size       : int;       (* total # of squares on board *)
        placed     : loc list;  (* locations of all knights placed on board *)
        last_index : int;       (* index of last knight placed *)
        indices    : S.t;
        reachable  : S.t
      }

    (* Helper functions. *)

    let check_bounds board loc = 
      ok_loc board.nrows board.ncols loc
    
    (*List of tuples of all the possible next moves for a knight*)
    let next_moves = 
      [(-2, -1); (-2, 1); (-1, -2); (-1, 2); (1, -2); (1, 2); (2, -1); (2, 1)] 

    let init_reachable nrows ncols =
      (*goes through all of next moves and counts how many are at ok location*)
      let rec count_reach (row, col) count next = 
        match next with 
        |[] -> count 
        |(r, c) :: t when ok_loc nrows ncols (row + r, col + c) -> 
          count_reach (row, col) (count + 1) t 
        |_ :: t -> count_reach (row,col) count t 
      in
      (*iterates through storage updating with correct reachability counts*)
      let rec iter row col store = 
        if col = ncols then store 
        else if row = nrows then iter 0 (col + 1) store 
        else 
          let new_store = 
            S.set store (row, col) (count_reach (row, col) 0 next_moves) in 
          iter (row + 1) col new_store 
      in
      iter 0 0 (S.make nrows ncols) 

    (* Interface functions. *)

    let make nrows ncols = 
      {
        nrows      = nrows;
        ncols      = ncols;
        size       = nrows * ncols;
        placed     = [];
        last_index = 0;
        indices    = S.make nrows ncols;
        reachable  = init_reachable nrows ncols
      }

    let get_dimensions board =
      (board.nrows, board.ncols)

    let get_last board =
      match board.placed with
        | [] -> raise Not_found
        | h :: _ -> h

    let get_index board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_index" "location off board" loc
      else
        S.get board.indices loc

    let get_reachable board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_reachable" "location off board" loc
      else
        S.get board.reachable loc

    (*Helper function: Returns a list of all the possible next locations for 
    knight given starting position and board*)
    let next_location board (row, col) = 
      let rec iter ans next = 
        match next with 
        |[] -> ans 
        |(r,c)::t when check_bounds board (row + r, col + c)-> 
          iter ((row + r, col + c) :: ans) t  
        |_ :: t -> iter ans t
      in
      iter [] next_moves 

    let get_loc_counts_from_loc board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_loc_counts_from_loc" "location off board" loc
      else 
        let rec iter ls next_loc = 
          match next_loc with 
          |[] -> ls 
          |h::t-> 
            begin 
              match get_reachable board h with 
              |None -> iter ls t 
              |Some x -> iter ((h, x):: ls) t
            end 
        in
        iter [] (next_location board loc) 

        let place board loc =
      (*Checks if last move to current move is a knight's move*)
          let is_knight_move (row, col) = 
            let (r, c) = get_last board in 
            let d1 = abs (row - r) in 
            let d2 = abs (col - c) in 
            (d1 = 2 && d2 = 1) || (d2 = 2 && d1 = 1)
            in
          if not(check_bounds board loc) then
            bad_loc "place" "location off board" loc
          else if get_index board loc <> None then
            bad_loc "place" "location already occupied" loc
          else if board.last_index <> 0 && 
            not (is_knight_move loc) then
            bad_loc "place" "loc not Knight's move" loc
          else 
            (*Update*)
            let placed = loc :: board.placed in
            let last_index = board.last_index + 1 in
            let indices = S.set board.indices loc last_index in
            (*Decrements at reachable locations*)
            let rec update r_board next = 
              match next with 
              |[] -> r_board 
              |h::t -> 
                begin 
                  match get_reachable board h with 
                  |None -> update r_board t 
                  |Some x -> update (S.set r_board h (x - 1)) t
                end 
              in
            (*Update reachable using update helper, 
              remove function makes location null*)
            let reachable = 
              update (S.remove board.reachable loc) 
              (next_location board loc)
              in
            {board with placed; last_index; indices; reachable}
      

    let undo board = 
      if board.last_index = 0 then board
      else
        let last_loc = get_last board in
        (*Update*)
        (*get tail of placed list ie. removing first element*)
        let placed = List.tl board.placed in
        let last_index = board.last_index - 1 in
        let indices = S.remove board.indices last_loc in
        let next_loc = next_location board last_loc in
        (*counts reachable locations *)
        let rec get_reach count next =
          match next with
            | [] -> count
            | h :: t when get_index board h = None ->
               get_reach (count + 1) t
            |_ :: t -> get_reach count t 
        in
        (*updates the reachability grid*)
        let rec update r_board next = 
          match next with 
          |[] -> r_board 
          |h::t -> 
            begin 
              match get_reachable board h with 
              |None -> update r_board t 
              |Some x -> update (S.set r_board h (x + 1)) t
            end 
          in
        let count = get_reach 0 next_loc in
        (*Update reachability grid from last location*)
        let reachable = 
          update (S.set board.reachable last_loc count) next_loc in
        {board with placed; last_index; indices; reachable}

    let is_solved board = board.last_index = board.size
    let get_placed board = List.rev board.placed
  end
