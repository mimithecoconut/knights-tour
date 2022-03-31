(* Knight's tour board representation. *)

open Storage

(**
 * The board module type and module.  
 * It represents the state of the knight's tour solution.
 *)

module type Board =
  sig
    type loc = Loc.t
    type t

    (** Make a board with a given number of rows and columns.
        If either is <= 0, raise `Invalid_argument`. *)
    val make : int -> int -> t

    (** Get the row/column dimensions of the board. *)
    val get_dimensions : t -> int * int

    (** Get the location of the last knight placed on the board.
        Raise `Not_found` if no knight has been placed. *)
    val get_last : t -> loc

    (** Get the placement number on the board at a given loc.
        Raise `Invalid_argument` if the loc isn't on the board.
        Return `None` if the loc is unoccupied. *)
    val get_index : t -> loc -> int option

    (** Get the reachable count on the board at a given loc.
        Raise `Invalid_argument` if the loc isn't on the board.
        Return `None` if the loc is unoccupied. *)
    val get_reachable : t -> loc -> int option

    (** Starting from a given loc, get the knights-move reachable locs
        along with the reachable count of each loc.
        Raise `Invalid_argument` if the loc isn't on the board. *)
    val get_loc_counts_from_loc : t -> loc -> (loc * int) list

    (** Place a knight on the board at a given Loc.
        Raise `Invalid_argument` if:
          a) the loc isn't on the board
          b) the loc is already occupied
          c) the loc isn't a knight's move away from the last placed knight
        Return the new board. *)
    val place : t -> loc -> t

    (** Undo the last move.  If there is no last move, do nothing. *)
    val undo : t -> t 

    (** Return `true` if the board is solved i.e. has a knight's tour on it. 
        The knight's tour can be open or closed. *)
    val is_solved : t -> bool

    (** Return the list of placed knights. *)
    val get_placed : t -> loc list
  end

module Make: functor (S: Storage) -> Board

