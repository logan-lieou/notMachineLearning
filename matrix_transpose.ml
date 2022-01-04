(* transpose *)
let rec transpose m = 
  assert (m <> []);
  if List.mem [] m then
    []
  else
    List.map List.hd m :: transpose (List.map List.tl m)
;;

let x = transpose [[1;2;3;4];[5;7;4;3]];;

let () = List.iter (List.iter (Printf.printf "%d ")) x;;
