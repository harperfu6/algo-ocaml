let rec print_ocaml_line m =
  if m = 0 then print_newline()
  else begin
    print_string "OCaml ";
    print_ocaml_line (m-1)
   end;;

let rec print_ocaml (n, m) =
  if n = 0 then print_newline()
  else begin
    print_ocaml_line m;
    print_ocaml (n-1,m)
  end;;
