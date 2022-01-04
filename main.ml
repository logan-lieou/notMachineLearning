(* insertion sort *)
let rec insert lst x =
  match lst with
    [] -> [x]
  | y :: ys when x <= y -> x :: y :: ys
  | y :: ys -> y :: insert ys x
;;

let insertion_sort = List.fold_left insert [];;
let x = insertion_sort [6; 7; 5; 8; 9; 2; 1; 4; 2; 3; 1];;

let () = List.iter (Printf.printf "%d\n") x;;
