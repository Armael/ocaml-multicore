open Lambda

let is_atom tm =
  match tm with
  | Lvar _ | Lconst _ | Lprim (_, []) ->
      true
  | _ ->
      false

let rec cps tm =
  match tm with
  | Lvar _ ->
      let k = Ident.create "k" in
      Lfunction {
        kind = Curried;
        params = [k];
        body = Lapply (Lvar k, [tm], no_apply_info)
      }
  | Lconst _ ->
      let k = Ident.create "k" in
      Lfunction {
        kind = Curried;
        params = [k];
        body = Lapply (Lvar k, [tm], no_apply_info)
      }
  | Lapply (f, args, info) ->
      let k = Ident.create "k" in
      let fv = Ident.create "f" in
      let args_idents = List.map (fun _ -> Ident.create "v") args in
      let final_apply = Lapply (Lvar fv,
                                List.map (fun i -> Lvar i) (args_idents @ [k]),
                                info) in
      Lfunction {
        kind = Curried;
        params = [k];
        body = cps_apply_body
            final_apply
            (List.combine (fv :: args_idents) (f :: args));
      }
  | Lfunction { kind; params; body } ->
      (* How do we handle kind = Tupled ? *)
      List.fold_right (fun v acc ->
        let k = Ident.create "k" in
        Lfunction {
          kind = Curried;
          params = [k];
          body =
            Lapply (Lvar k,
                    [Lfunction {
                       kind = Curried;
                       params = [v];
                       body = acc
                     }],
                    no_apply_info)
        }
      ) params (cps body)
  | Llet (kind, ident, e1, e2) ->
      cps_let ident e1 e2
  | Lletrec (decl, body) ->
      (* works because identifiers cannot clash (otherwise, the continuation
         [k] could accidentaly capture variables bound by [decl]) *)
      let k = Ident.create "k" in
      let decl_cps = List.map (fun (i, t) ->
        (i, Ident.create ("x" ^ i.Ident.name), cps t)) decl in
      let body =
        List.fold_right (fun (i, xi, cps_t) acc ->
          Lapply (Lvar xi,
                  [Lfunction { kind = Curried; params = [i]; body = acc }],
                  no_apply_info)
        ) decl_cps (Lapply (cps body, [Lvar k], no_apply_info)) in

      Lfunction {
        kind = Curried;
        params = [k];
        body =
          Lletrec (
            List.map (fun (_, xi, t) -> (xi, t)) decl_cps,
            body
          )
      }

  | Lprim (prim, args) ->
      let k = Ident.create "k" in
      let args_idents = List.map (fun _ -> Ident.create "v") args in
      let final_apply =
        Lapply (Lvar k,
                [Lprim (prim, List.map (fun i -> Lvar i) args_idents)],
                no_apply_info) in
      Lfunction {
        kind = Curried;
        params = [k];
        body = cps_apply_body final_apply (List.combine args_idents args)
      }
  | Lsequence (e1, e2) ->
      let dummy = Ident.create "_" in
      cps_let dummy e1 e2
  | _ -> failwith "not handled" 

and cps_apply_body final_apply args =
  List.fold_left (fun acc (v, arg) ->
    lambda_apply arg v acc
  ) final_apply args
  
and cps_let ident e1 e2 =
  let k = Ident.create "k" in
  Lfunction {
    kind = Curried;
    params = [k];
    body = lambda_apply e1 ident (Lapply (cps e2, [Lvar k], no_apply_info));
  }

and lambda_apply e x body_cps =
  if is_atom e then (
    subst_lambda (Ident.add x e Ident.empty) body_cps
  ) else (
    Lapply (
      cps e,
      [Lfunction {
         kind = Curried;
         params = [x];
         body = body_cps;
       }],
      no_apply_info
    )
  )

let toplevel_cps tm =
  let x = Ident.create "x" in
  let identity = Lfunction { kind = Curried; params = [x]; body = Lvar x } in

  match tm with
  | Lprim (Psetglobal ident, [body]) ->
      Lprim (Psetglobal ident, [
        Lapply (cps body, [identity], no_apply_info)
      ])
  | _ -> assert false

let toplevel_cps tm =
  let x = Ident.create "x" in
  let identity = Lfunction { kind = Curried; params = [x]; body = Lvar x } in

  Lapply (cps tm, [identity], no_apply_info)

