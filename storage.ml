(*Student name: Shenyi Li*)
(*Email: sli5@caltech.edu*)

module Loc =
  struct
    type t = int * int

    let compare = Stdlib.compare
  end

module type Storage =
  sig
    type t
    type loc = Loc.t

    val make    : int -> int -> t
    val get     : t -> loc -> int option
    val set     : t -> loc -> int -> t
    val has_loc : t -> loc -> bool
    val remove  : t -> loc -> t 
  end

(*
 * Imperative implementation.
 * The data representation is an array of arrays of integers.
 * A null location is represented by the number -1 stored at the location.
 *)
module ImpStorage : Storage =
  struct
    type t   = int array array
    type loc = Loc.t

    let make nrows ncols = 
      if nrows < 1 || ncols < 1 
        then invalid_arg 
        (Printf.sprintf "make: invalid arguments: nrows = %d, ncols = %d"
        nrows ncols)
      else 
        Array.make_matrix nrows ncols (-1) 


    let get data (row, col) = 
      try 
        let stored = data.(row).(col) in 
        if stored = -1 then None 
        else Some stored 
      with Invalid_argument _ -> None  

    let set data (row, col) i = 
      if i < 0 then invalid_arg "set: negative aurgment"
      else 
        try 
          data.(row).(col) <- i; 
          data 
        with Invalid_argument _ -> 
          invalid_arg 
          (Printf.sprintf "set: invalid location: (%d, %d)" row col) 

    let has_loc data (row, col) =
      try 
        if data.(row).(col) == -1 then false 
        else true  
      with Invalid_argument _ -> false  

    let remove data (row, col) =
      try 
        data.(row).(col) <- (-1);
        data
      with Invalid_argument _ -> data 
  end

(*
 * Functional implementation.
 * The data representation is a map between locs and integers.
 * A null location is represented by the absence of the loc in the map.
 *)
module FunStorage : Storage =
  struct
    module LocMap = Map.Make(Loc)

    type t = 
      {
        contents : int LocMap.t;
        nrows    : int;
        ncols    : int
      }

    type loc = Loc.t

    let make nrows ncols = 
      if nrows < 1 || ncols < 1 
        then invalid_arg 
          (Printf.sprintf "make: invalid arguments: nrows = %d, ncols = %d"
        nrows ncols)
      else {contents = LocMap.empty; nrows; ncols}
    
    let get data (row, col) = 
      if row < 0 || col < 0 || row >= data.nrows || col >= data.ncols 
        then None 
      else 
        try 
          Some (LocMap.find (row, col) data.contents)
        with Not_found -> None  

    let set data (row, col) i = 
      if row < 0 || col < 0 || row >= data.nrows || col >= data.ncols 
        then invalid_arg 
        (Printf.sprintf "set: invalid location: (%d, %d)" row col) 
      else if i < 0 
        then invalid_arg "set: negative aurgment"
      else
        {data with contents =
         (LocMap.update (row, col) (fun _ -> Some i) data.contents)}      

    let has_loc data (row, col) = 
      LocMap.mem (row, col) data.contents 

    let remove data (row, col) =
        {data with contents = (LocMap.remove (row, col) data.contents)}
  end

