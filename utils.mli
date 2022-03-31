open Storage
open Board

module Print : functor (B : Board) -> 
  sig 
    (* Print the board.  If the bool argument is true, print both the
     * index board and the reachability board. *)
    val print_board : B.t -> bool -> unit 
  end

(** Check a solution represented as a list of (row, column) locations.
    The int arguments are the number of rows and columns, respectively. 
    Return `true` if the solution is valid. *)
val check_solution : int -> int -> Loc.t list -> bool

