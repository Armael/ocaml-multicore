open Lambda

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
      let f_cps = cps f in
      let args_idents = List.map (fun _ -> Ident.create "v") args in
      let args_cps = List.map cps args in
      let final_apply = Lapply (Lvar fv,
                                List.map (fun i -> Lvar i) (args_idents @ [k]),
                                info) in
      Lfunction {
        kind = Curried;
        params = [k];
        body = cps_apply_body
            final_apply
            (List.combine (fv :: args_idents) (f_cps :: args_cps));
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
  | Lprim (prim, args) ->
      let k = Ident.create "k" in
      let args_idents = List.map (fun _ -> Ident.create "v") args in
      let args_cps = List.map cps args in
      let final_apply =
        Lapply (Lvar k,
                [Lprim (prim, List.map (fun i -> Lvar i) args_idents)],
                no_apply_info) in
      Lfunction {
        kind = Curried;
        params = [k];
        body = cps_apply_body final_apply (List.combine args_idents args_cps)
      }
  | Lsequence (e1, e2) ->
      let dummy = Ident.create "_" in
      cps_let dummy e1 e2
  | _ -> failwith "not handled" 

and cps_apply_body final_apply args_cps =
  List.fold_left (fun acc (v, arg_cps) ->
    Lapply (arg_cps,
            [Lfunction { kind = Curried;
                         params = [v];
                         body = acc }],
            no_apply_info)
  ) final_apply args_cps
  
and cps_let ident e1 e2 =
  let k = Ident.create "k" in
  Lfunction {
    kind = Curried;
    params = [k];
    body =
      Lapply (cps e1,
              [Lfunction {
                 kind = Curried;
                 params = [ident];
                 body = Lapply (cps e2, [Lvar k], no_apply_info)
               }],
              no_apply_info)
  }


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

