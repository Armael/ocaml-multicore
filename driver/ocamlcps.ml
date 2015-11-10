open Lambda

let f = Ident.create "f"
let x = Ident.create "x"
let y = Ident.create "y"
let v = Ident.create "v"
let a = Ident.create "a"
let b = Ident.create "b"
let exn = Ident.create "exn"
let eff = Ident.create "eff"
let k = Ident.create "k"
let hv = Ident.create "hv"
let he = Ident.create "he"
let hf = Ident.create "hf"

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

let func2 =
  Lfunction (
    Curried,
    [x; y],
    Lvar a
  )

let apply1 =
  Lapply (Lvar f, [Lvar a], Location.none)

let apply2 =
  Lapply (Lvar f, [Lvar a; Lvar b], Location.none)

let caml_alloc_stack =
  Lprim (
    (Pccall { Primitive.prim_name = "caml_alloc_stack";
              Primitive.prim_arity = 1;
              Primitive.prim_alloc = true;
              Primitive.prim_native_name = "";
              Primitive.prim_native_float = false }),
    [Lfunction (Curried, [x], Lvar hv);
     Lfunction (Curried, [exn], Lvar he);
     Lfunction (Curried, [eff; k], Lvar hf)]
  )

let resume =
  Lprim (Presume, [caml_alloc_stack; Lvar f; Lvar v])

let _ =
  List.iter (fun tm ->
    Printlambda.lambda Format.std_formatter tm;
    Format.printf "\n\n%!";
    Printlambda.lambda Format.std_formatter (Cps.cps tm);
    Format.printf "\n\n-------------------------\n\n%!"
  ) [var; func; func2; apply1; apply2; seq; prim0; prim1; prim2; apply_prim; resume]
