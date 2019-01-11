open Gfile

(* Used to randomly generate a graph given the number of nodes wanted and the name of the output file that will be readable by ftest *)

let () =

  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf "\nUsage: output number_of_nodes\n\n%!";
      exit 0
    end ;

  let output = (Sys.argv.(1))
  in
  let num = int_of_string (Sys.argv.(2))
  in
  let gr = generate_graph num
  in
  let path = output
  in
  let () = Gfile.write_file path gr
  in
  ()
