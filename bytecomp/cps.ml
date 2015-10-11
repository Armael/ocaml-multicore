open Lambda

let is_atom tm =
  match tm with
  | Lvar _ | Lconst _ -> true
  | _ -> false

module C : sig
  type lambda_cps = private lambda
  type cont_ident
  type cont

  type k =
    | Cid of Ident.t
    | Clambda of Ident.t * lambda

  val create_cont_ident : string -> cont_ident
  val std : cont_ident -> Ident.t
  val err : cont_ident -> Ident.t

  val mkcont :
    ?std:k ->
    ?err:k ->
    cont_ident ->
    cont

  val abs_cont : cont_ident -> lambda -> lambda_cps
  val continue_with : cont -> lambda_cps -> lambda

  val assert_cps : lambda -> lambda_cps

end = struct
  type lambda_cps = lambda
  type cont_ident = Ident.t * Ident.t
  type k =
    | Cid of Ident.t
    | Clambda of Ident.t * lambda

  let lambda_of_k = function
    | Cid k -> Lvar k
    | Clambda (i, t) ->
        Lfunction { kind = Curried; params = [i]; body = t }

  type cont = k * k

  let create_cont_ident name =
    (Ident.create (name ^ "k")),
    (Ident.create (name ^ "ke"))

  let std (k, ke) =
    k

  let err (k, ke) =
    ke

  let mkcont ?std ?err (k, ke) =
    let mkk o k = match o with
      | None -> Cid k
      | Some k -> k in
    (mkk std k, mkk err ke)

  let abs_cont (k, ke) t =
    Lfunction {
      kind = Curried;
      params = [k; ke];
      body = t;
    }

  let continue_with (c, ce) tm =
    match tm, c, ce with
    | Lfunction {
      kind;
      params = [k; ke];
      body = Lapply (Lvar k', [atom], _)
    }, c, ce
      when k = k' && is_atom atom ->
        begin match c with
        | Cid k ->
            Lapply (Lvar k, [atom], no_apply_info)
        | Clambda (v, vcont) ->
            subst_lambda (Ident.add v atom Ident.empty) vcont
        end
    | Lfunction { kind; params = [k; ke]; body }, Cid ck, Cid ce ->
        subst_lambda
          (Ident.empty |> Ident.add k (Lvar ck) |> Ident.add ke (Lvar ce))
          body
    | Lapply (f, args, info), c, ce ->
        Lapply (f, args @ [lambda_of_k c; lambda_of_k ce], info)
    | t, c, ce ->
        Lapply (t, [lambda_of_k c; lambda_of_k ce], no_apply_info)

  let assert_cps t =
    t
end

open C

let static_handlers = ref []

let rec cps_eval_chain
    (default_cont : cont_ident)
    (id_tms: (Ident.t * lambda_cps) list)
    (body: lambda):
  lambda
  =
  List.fold_right (fun (id, tm) acc ->
    continue_with
      (mkcont ~std:(Clambda (id, acc)) default_cont)
      tm
  ) id_tms body

and cps (tm: lambda): lambda_cps =
  match tm with
  | _ when is_atom tm ->
      (*
         ⟦a⟧ = λk ke. k a
      *)
      let k = create_cont_ident "" in
      abs_cont k (Lapply (Lvar (std k), [tm], no_apply_info))

  | Lapply (f, args, info) ->
      (*
         ⟦a b⟧ = λk ke. ⟦a⟧ (λva. ⟦b⟧ (λvb. va vb k ke) ke) ke
      *)
      let k = create_cont_ident "" in
      let fv = Ident.create "f" in
      let args_cps = List.map cps args in
      let args_idents = List.map (fun _ -> Ident.create "v") args in
      let final_apply =
        continue_with
          (mkcont k)
          (assert_cps
             (Lapply (Lvar fv,
                      List.map (fun i -> Lvar i) args_idents,
                      info)))
      in
      abs_cont k
        (cps_eval_chain k
           (List.combine (fv :: args_idents) (cps f :: args_cps))
           final_apply)

  | Lfunction { kind; params; body } ->
      (*
         ⟦λx.a⟧ = λk ke. k (λx. ⟦a⟧)
      *)
      (* How do we handle kind = Tupled ? *)
      List.fold_right (fun v (acc: lambda_cps) ->
        let k = create_cont_ident "" in
        abs_cont k
          (Lapply (Lvar (std k),
                   [Lfunction {
                      kind = Curried;
                      params = [v];
                      body = (acc :> lambda)
                    }],
                   no_apply_info))
      ) params (cps body)

  | Llet (kind, ident, e1, e2) ->
      (*
         ⟦let x = a in b⟧ = λk ke. ⟦a⟧ (λx. ⟦b⟧ k ke) ke
      *)
      let k = create_cont_ident "" in
      abs_cont k
        (cps_eval_chain k
           [(ident, cps e1)]
           (continue_with (mkcont k) (cps e2)))

  | Lsequence (e1, e2) ->
      (*
         ⟦a; b⟧ = λk ke. ⟦a⟧ (λ_. ⟦b⟧ k ke) ke
      *)
      let dummy = Ident.create "_" in
      cps (Llet (Strict, dummy, e1, e2))

  | Lletrec (decl, body) ->
      (*
         ⟦let rec
            x₁ = a₁
            …
            xn = an
          in b⟧
         =
         λk ke. ⟦a₁⟧ (λva₁. … ⟦an⟧ (λvan.
                       let rec x₁ = va₁ … xn = van in ⟦b⟧ k ke)… ke)
                     ke
      *)
      let k = create_cont_ident "" in
      let decl_idents = List.map (fun (i, t) ->
        (i, Ident.create ("x" ^ i.Ident.name), t)) decl in
      let decl_cps = List.map (fun (i, xi, t) ->
        (xi, (cps t : lambda_cps :> lambda))
      ) decl_idents in

      abs_cont k
        (Lletrec (
           decl_cps,
           (cps_eval_chain k
              (List.map (fun (i, xi, _) -> (i, assert_cps (Lvar xi))) decl_idents)
              (continue_with (mkcont k) (cps body)))
         ))

  | Lprim (Praise _ (* ? *), [e]) ->
      (*
         ⟦raise e⟧ = λk ke. ⟦e⟧ ke ke
      *)
      let k = create_cont_ident "" in
      abs_cont k
        (continue_with
           (mkcont ~std:(Cid (err k)) k)
           (cps e))

  | Lprim (prim, args) ->
      (*
         ⟦prim a⟧ = λk ke. ⟦a⟧ (λva. k (prim va)) ke
      *)
      let k = create_cont_ident "" in
      let args_cps = List.map cps args in
      let args_idents = List.map (fun _ -> Ident.create "v") args in
      let final_apply =
        Lapply (
          Lvar (std k),
          [Lprim (prim, List.map (fun i -> Lvar i) args_idents)],
          no_apply_info
        ) in
      abs_cont k
        (cps_eval_chain k
           (List.combine args_idents args_cps)
           final_apply)

  | Ltrywith (body, exn, handle) ->
      (*
         ⟦try a with e -> b⟧ = λk ke. ⟦a⟧ k (λe. ⟦b⟧ k ke)
      *)
      let k = create_cont_ident "" in
      abs_cont k
        (continue_with
           (mkcont
              ~err:(Clambda (exn, continue_with (mkcont k) (cps handle)))
              k)
           (cps body))

  | Lstaticcatch (body, (lbl, args), handle) ->
      (*
         ⟦catch a with lbl, args -> b⟧ = λk ke. ⟦λargs. b⟧ (λh. ⟦a⟧ k ke) ke
         + register (lbl → h) in [static_handlers]
      *)
      let k = create_cont_ident "" in
      let handler = Ident.create "handler" in
      static_handlers := (lbl, handler) :: !static_handlers;
      let args = if args = [] then [Ident.create "_"] else args in
      abs_cont k
        (continue_with
           (mkcont
              ~std:(Clambda (handler, continue_with (mkcont k) (cps body)))
              k)
           (cps (Lfunction { kind = Curried; params = args; body = handle })))

  | Lstaticraise (lbl, args) ->
      (*
         ⟦exit lbl args⟧ = ⟦h args⟧
         when (lbl → h) is in [static_handlers]
      *)
      let handler = List.assoc lbl !static_handlers in
      let args = if args = [] then [Lconst const_unit] else args in
      cps (Lapply (Lvar handler, args, no_apply_info))

  | Lifthenelse (cond, thenbody, elsebody) ->
      (*
         ⟦if c then a else b⟧
         =
         λk ke. ⟦c⟧ (λvc. (if vc then ⟦a⟧ else ⟦b⟧) k ke) ke
      *)
      let k = create_cont_ident "" in
      let condv = Ident.create "cond" in
      let body = continue_with (mkcont k)
          (Lifthenelse (
             Lvar condv,
             (cps thenbody : lambda_cps :> lambda),
             (cps elsebody : lambda_cps :> lambda)
           ) |> assert_cps)
      in

      abs_cont k
        (continue_with
           (mkcont ~std:(Clambda (condv, body)) k)
           (cps cond))

  | Lswitch (t, { sw_numconsts;
                  sw_consts;
                  sw_numblocks;
                  sw_blocks;
                  sw_failaction }) ->
      (*
         ⟦switch a case c₁ -> b₁ | … | cn -> bn⟧
         =
         λk ke. ⟦a⟧ (λva. (switch va case c₁ -> ⟦b₁⟧ | … | cn -> ⟦bn⟧) k ke) ke
      *)
      let k = create_cont_ident "" in
      let tv = Ident.create "v" in
      let cps_case (i, t) = i, (cps t : lambda_cps :> lambda) in
      let sw_consts = List.map cps_case sw_consts in
      let sw_blocks = List.map cps_case sw_blocks in
      let sw_failaction = match sw_failaction with
        | Some t -> Some (cps t : lambda_cps :> lambda)
        | None -> None in
      let body = continue_with (mkcont k)
          (Lswitch (Lvar tv, { sw_numconsts;
                               sw_consts;
                               sw_numblocks;
                               sw_blocks;
                               sw_failaction })
           |> assert_cps)
      in
      abs_cont k
        (continue_with
           (mkcont ~std:(Clambda (tv, body)) k)
           (cps t))

  | Lstringswitch (t, clauses, failaction) ->
      let k = create_cont_ident "" in
      let tv = Ident.create "v" in
      let cps_case (s, t) = s, (cps t : lambda_cps :> lambda) in
      let failaction = match failaction with
        | Some t -> Some (cps t : lambda_cps :> lambda)
        | None -> None in
      let body = continue_with (mkcont k)
          (Lstringswitch (Lvar tv,
                          List.map cps_case clauses,
                          failaction)
           |> assert_cps)
      in
      abs_cont k
        (continue_with
           (mkcont ~std:(Clambda (tv, body)) k)
           (cps t))

  | _ -> failwith "unhandled"

(* let toplevel_cps tm = *)
(*   let x = Ident.create "x" in *)
(*   let identity = Lfunction { kind = Curried; params = [x]; body = Lvar x } in *)

(*   match tm with *)
(*   | Lprim (Psetglobal ident, [body]) -> *)
(*       Lprim (Psetglobal ident, [ *)
(*         Lapply (cps body, [identity], no_apply_info) *)
(*       ]) *)
(*   | _ -> assert false *)

let cps tm =
  ((cps tm) :> lambda)

let toplevel_cps tm =
  (* TODO: proper uncaught exception handler *)
  let x = Ident.create "x" in
  let identity = Lfunction { kind = Curried; params = [x]; body = Lvar x } in

  Lapply (
    (cps tm :> lambda),
    [identity; identity],
    no_apply_info
  )
