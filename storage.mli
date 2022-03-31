(* Board representations. *)

(** Module representing (row, column) locations. *)
module Loc :
  sig 
    type t = int * int 
    val compare : t -> t -> int 
  end

(** Module representing a 2D grid of (row, column) locations
    where some of the locations in the grid can be unoccupied
    i.e. not actually present on the grid even though they
    are inside the grid. *)
module type Storage =
  sig
    (** Type of the board. *)
    type t

    (** Type of locations on the board. *)
    type loc = Loc.t

    (** Make a board with a given number of rows and columns.
        All locations start out unoccupied.
        If either is <= 0, raise `Invalid_argument`. *)
    val make : int -> int -> t

    (** Get the number at a given loc. 
        If the loc isn't stored, return `None`. *)
    val get : t -> loc -> int option

    (** Set the number on the board at a given loc. 
        If the loc is off the board or if the number is negative, 
          raise `Invalid_argument`.
        Return the new storage.  
        The input storage may or may not be altered 
          (implementation dependent). *)
    val set : t -> loc -> int -> t

    (** Check if a loc is in storage. *)
    val has_loc : t -> loc -> bool

    (** Remove a loc from storage. 
        Do nothing if it isn't in storage.
        The input storage may or may not be altered 
          (implementation dependent). *)
    val remove : t -> loc -> t 
  end

module ImpStorage : Storage
module FunStorage : Storage
