type expression =
      Const of float
    | Var of string
    | Sum of expression * expression    (* e1 + e2 *)
    | Diff of expression * expression   (* e1 - e2 *)
    | Prod of expression * expression   (* e1 * e2 *)
    | Quot of expression * expression   (* e1 / e2 *)
  ;;

let printer exp =
  (* Local function def *)
  let open_paren prec op_prec =
    if prec > op_prec then print_string "(" in
  let close_paren prec op_prec =
    if prec > op_prec then print_string ")" in
  (* prec = current precedence *)
  let rec print prec exp = 
    match exp with
      Const c -> print_float c
    | Var v -> print_string v
    | Sum (f, g) ->
        open_paren prec 0;
        print 0 f; print_string " + "; print 0 g;
        close_paren prec 0
    | Diff (f, g) -> 
        open_paren prec 0;
        print 0 f; print_string " - "; print 1 g;
        close_paren prec 0;
    | Prod (f, g) -> 
        open_paren prec 2;
        print 2 f; print_string " * "; print 2 g;
        close_paren prec 2;
    | Quot (f, g) -> 
        open_paren prec 2;
        print 2 f; print_string " / "; print 3 g;
        close_paren prec 2
  in print 0 exp;;
