open Lambda

let f = Ident.create "f"
let x = Ident.create "x"
let a = Ident.create "a"
let b = Ident.create "b"

let apply_prim =
  Lapply (
    Lprim (Pfield 29, [Lvar a]),
    [Lvar b],
    no_apply_info
  )

let prim0 =
  Lprim (Pfield 0, [])

let prim1 =
  Lprim (Phandle, [Lvar a])

let prim2 =
  Lprim (Pisint, [Lprim (Pisint, []); Lprim (Pisout, []); Lvar a])

let seq =
  Lsequence (Lvar a, Lvar b)

let var =
  Lvar a

let func =
  Lfunction {
    kind = Curried;
    params = [x];
    body = Lvar a
  }

let apply1 =
  Lapply (Lvar f, [Lvar a], no_apply_info)

let apply2 =
  Lapply (Lvar f, [Lvar a; Lvar b], no_apply_info)

let _ =
  List.iter (fun tm ->
    Printlambda.lambda Format.std_formatter tm;
    Format.printf "\n\n%!";
    Printlambda.lambda Format.std_formatter (Cps.cps tm);
    Format.printf "\n\n-------------------------\n\n%!"
  ) [var; func; apply1; apply2; seq; prim0; prim1; prim2; apply_prim]
