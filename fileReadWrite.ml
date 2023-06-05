

(* Read .txt file and return list *)
let read_file1 filename =
  let file = open_in filename in
  let rec loop acc =
    try
      let line = input_line file in
      loop (line :: acc)
    with
      End_of_file -> List.rev acc
  in
  let lines = loop [] in
  close_in file;
  lines


  (* write some message in already existing file *)
  (* can also create file if filename is not found *)
  let write_file filename lines =
    let file = open_out filename in
    List.iter (fun line -> output_string file (line ^ "\n")) lines;
    close_out file



  (* output contents of file and print *)
  let read_file2 filename =
    let input_channel = open_in filename in
    try
      while true do
        let line = input_line input_channel in
         print_endline line
      done
    with
      End_of_file ->
        close_in input_channel









        let read_line (input, position) =
          let rec read_line_aux acc i =
            if i >= String.length input || input.[i] = '\n' then
              ((input, i + 1), String.concat "" (List.rev acc))
            else
              read_line_aux ((String.sub input i 1) :: acc) (i + 1)
          in
          read_line_aux [] position