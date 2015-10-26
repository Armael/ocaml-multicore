open Lambda

let f = Ident.create "f"
let x = Ident.create "x"
let a = Ident.create "a"
let b = Ident.create "b"

let apply_prim =
  Lapply (
    Lprim (Pfloatfield 29, [Lvar a]),
    [Lvar b],
    Location.none
  )

let prim0 =
  Lprim (Pfloatfield 0, [])

let prim1 =
  Lprim (Pperform, [Lvar a])

let prim2 =
  Lprim (Pisint, [Lprim (Pisint, []); Lprim (Pisout, []); Lvar a])

let seq =
  Lsequence (Lvar a, Lvar b)

let var =
  Lvar a

let func =
  Lfunction (
    Curried,
    [x],
    Lvar a
  )

let apply1 =
  Lapply (Lvar f, [Lvar a], Location.none)

let apply2 =
  Lapply (Lvar f, [Lvar a; Lvar b], Location.none)

let _ =
  List.iter (fun tm ->
    Printlambda.lambda Format.std_formatter tm;
    Format.printf "\n\n%!";
    Printlambda.lambda Format.std_formatter (Cps.cps tm);
    Format.printf "\n\n-------------------------\n\n%!"
  ) [var; func; apply1; apply2; seq; prim0; prim1; prim2; apply_prim]
