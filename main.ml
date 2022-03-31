open Storage
open Utils
open Printf

module SI = Search.Make(ImpStorage)
module SF = Search.Make(FunStorage)

let runSI nrows ncols start_row start_col print =
  begin
    Random.self_init ();
    match SI.search nrows ncols start_row start_col print with
      | None -> printf "Solution not found :(\n%!"
      | Some locs -> 
          if check_solution nrows ncols locs then
            printf "Valid solution found!\n%!"
          else
            printf "Invalid solution found :(\n%!"
  end

let runSF nrows ncols start_row start_col print =
  begin
    Random.self_init ();
    match SF.search nrows ncols start_row start_col print with
      | None -> printf "Solution not found :(\n%!"
      | Some locs -> 
          if check_solution nrows ncols locs then
            printf "Valid solution found!\n%!"
          else
            printf "Invalid solution found :(\n%!"
  end

let usage () = 
  begin
    fprintf stderr 
      "usage: ./knights_tour nrows ncols [-start m n] [-f] [-q]\n%!";
    fprintf stderr "  nrows: number of rows (int)\n%!";
    fprintf stderr "  ncols: number of columns (int)\n%!";
    fprintf stderr "  -start m n: starting square (int, int)\n%!";
    fprintf stderr "  -f: use functional board\n%!";
    fprintf stderr "  -q: don't print solution board\n%!"
  end

let get_sizes args =
  if Array.length args < 3 then
    None
  else
    Some (int_of_string args.(1), int_of_string args.(2))

let opt_start args =
  let lst = Array.to_list args in
  let rec iter lst =
    match lst with
      | []
      | [_]
      | [_; _] -> None
      | ("-start" :: ms :: ns :: _) ->
          let m = int_of_string ms in
          let n = int_of_string ns in
            Some (m, n)
      | _ :: t -> iter t
  in
    iter lst

let opt_f args = Array.exists (fun s -> s = "-f") args
let opt_q args = Array.exists (fun s -> s = "-q") args

let _ =
  try
    let args = Sys.argv in
      match get_sizes args with
        | None -> usage ()
        | Some (nrows, ncols) ->
            let functional = opt_f args in
            let quiet = opt_q args in
              match opt_start args with
                | None ->
                  if functional then
                    runSF nrows ncols 0 0 (not quiet)
                  else
                    runSI nrows ncols 0 0 (not quiet)
                | Some (m, n) ->
                  if functional then
                    runSF nrows ncols m n (not quiet)
                  else
                    runSI nrows ncols m n (not quiet)
  with Failure msg -> 
    usage ()

