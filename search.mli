open Storage
open Board

exception Solution_not_found

module type Searcher =
  sig
    (** Search for a solution given:
        -- number of rows
        -- number of columns
        -- starting row
        -- starting column
        -- flag: if true; print the solution if one is found
        Return the solution (as a list of locations) if found, else `None`. *)
    val search : int -> int -> int -> int -> bool -> (Loc.t list) option
  end

module Make(S : Storage.Storage) : Searcher 

