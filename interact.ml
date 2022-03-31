open Storage
open Board
open Utils

open Printf
open Scanf

module Interact (S : Storage) =
  struct
    module B = Board.Make(S)
    module P = Print(B)

    let rec get_dims () =
      try
        begin
          printf "Enter number of rows and columns: %!";
          let input = read_line () in
          let (nrows, ncols) = sscanf input "%d %d" (fun r c -> (r, c)) in
            if nrows <= 0 || ncols <= 0 then
              invalid_arg
                (Printf.sprintf 
                   "invalid dimensions: nrows = %d, ncols = %d%!" 
                   nrows ncols)
            else
              (nrows, ncols)
        end
      with 
        | Invalid_argument msg ->
            begin
              printf "ERROR: %s\n%!" msg;
              get_dims ()
            end
        | Scan_failure _ ->
            begin
              printf "ERROR: invalid input\n%!";
              get_dims ()
            end
        | End_of_file ->
            begin
              printf "ERROR: missing input\n%!";
              get_dims ()
            end

    let rec process_input board =
      try
        begin
          printf "interact> %!";
          let input = read_line () in
            if input = "q" then
              raise Exit
            else if input = "u" then
              B.undo board
            else
              let loc = sscanf input "%d %d" (fun r c -> (r, c)) in
                B.place board loc
        end
      with 
        | Invalid_argument msg ->
            begin
              printf "ERROR: %s\n%!" msg;
              process_input board
            end
        | Scan_failure _ ->
            begin
              printf "ERROR: invalid input\n%!";
              process_input board
            end
        | End_of_file ->
            begin
              printf "ERROR: missing input\n%!";
              process_input board
            end

    let interact () = 
      let (nrows, ncols) = get_dims () in
      let board = B.make nrows ncols in
      let b = ref board in
        try
          begin
            P.print_board !b true;
            while not (B.is_solved !b) do
              b := process_input !b;
              P.print_board !b true
            done;
            printf "Board is solved!\n%!"
          end
        with Exit -> ()

  end
    

module II = Interact(ImpStorage)
module IF = Interact(FunStorage)

let _ = II.interact ()

