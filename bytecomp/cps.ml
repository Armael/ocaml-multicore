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

let cps_eval_chain
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

let rec cps (already_cps: lambda -> bool) (tm: lambda): lambda_cps =
  let cps' = cps already_cps in
  match tm with
  | _ when already_cps tm -> assert_cps tm
  | Lvar _ | Lconst _ -> (* is_atom tm *)
      (*
         ⟦a⟧ = λk ke. k a
      *)
      let k = create_cont_ident "" in
      abs_cont k (Lapply (Lvar (std k), [tm], no_apply_info))

  | Lapply (f, args, info) ->
      (*
         ⟦a b⟧ = λk ke. ⟦a⟧ (λva. ⟦b⟧ (λvb. va vb k ke) ke) ke
      *)
      (* (observable) order of evaluation of f a1 ... an:
         an, ..., a1, f

         --> cps_eval_chain ~rev:true
      *)
      let k = create_cont_ident "" in
      let fv = Ident.create "f" in
      let args_cps = List.map cps' args in
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
           (List.combine (fv :: args_idents) (cps' f :: args_cps))
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
      ) params (cps' body)

  | Llet (kind, ident, e1, e2) ->
      (*
         ⟦let x = a in b⟧ = λk ke. ⟦a⟧ (λx. ⟦b⟧ k ke) ke
      *)
      let k = create_cont_ident "" in
      (* Let-bound references elimination is disabled. 
         See the comment for Lassign for more details.
      *)
      assert (kind <> Variable);
      abs_cont k
        (cps_eval_chain k
           [(ident, cps' e1)]
           (continue_with (mkcont k) (cps' e2)))

  | Lsequence (e1, e2) ->
      (*
         ⟦a; b⟧ = λk ke. ⟦a⟧ (λ_. ⟦b⟧ k ke) ke
      *)
      let dummy = Ident.create "_" in
      cps' (Llet (Strict, dummy, e1, e2))

  | Lletrec (decl, body) ->
      (*
         ⟦let rec
            x₁ = a₁
            …
            xn = aₙ
          in b⟧
         =
         λk ke.
           let rec
             xx₁ = ⟦a₁⟧[x₁ ← xx₁, xₙ ← xxₙ]
             …
             xxₙ = ⟦aₙ⟧[x₁ ← xx₁, xₙ ← xxₙ]
           in
           xx₁ (λx₁. … xxₙ (λxₙ. ⟦b⟧ k ke) ke)… ke
      *)
      (* Subtlety: the CPS translation of a1…an must take into account
         that xx1…xxn are already in cps form, and do not translate
         them as standard variables.  (thus the custom [already_cps]
         argument for [cps))

         XXX:
         This translation is only correct for the case of mutually
         recursive functions.

         Indeed, recursive definitions are translated to recursive
         definitions in CPS form. In the definition body of these
         definitions, the values themselves are used (as the
         definition are recursive): if evaluating the CPS term perform
         effects, these will be performed recursively.

         This does not match the semantics for recursive definitions
         of values. For example, 

         [let rec l = print_int 3; () :: l in ()]

         prints 3 once, then binds l to a value (that has a cycle).

         With the CPS translation presented earlier, this would be
         translated to a definition that loops, printing 3 an infinite
         number of times: l being translated to a CPS term, getting
         [() :: l] is translated to evaluating the [l] cps term, then
         consing the result after a [()]; which triggers the
         [print_int] and loops forever.

         
         An idea to handle the "recursive value" case would be to
         first extract all the side computations (such as [print_int
         3]), run them, and then compute the recursive value as usual,
         without trying to CPS its definition.
      *)
      if not (List.for_all (function (_, Lfunction _) -> true | _ -> false)
                decl) then
        failwith "Recursive values are not handled";

      let k = create_cont_ident "" in
      let decl_idents = List.map (fun (i, t) ->
        (i, Ident.create ("x" ^ i.Ident.name), t)) decl in
      let subst_i_xi = List.fold_left (fun subst (i, xi, _) ->
        Ident.add i (Lvar xi) subst
      ) Ident.empty decl_idents in
      let already_cps tm =
        (match tm with
        | Lvar v ->
          List.exists (fun (_, xi, _) -> Ident.same v xi) decl_idents
        | _ -> false)
        || already_cps tm
      in

      let decl_cps = List.map (fun (i, xi, t) ->
        let cps_t =
          subst_lambda subst_i_xi t
          |> cps already_cps
        in
        (xi, (cps_t : lambda_cps :> lambda))
      ) decl_idents in

      abs_cont k
        (Lletrec (
           decl_cps,
           (cps_eval_chain k
              (List.map (fun (i, xi, _) -> (i, assert_cps (Lvar xi))) decl_idents)
              (continue_with (mkcont k) (cps' body)))
         ))

  | Lprim (Praise _ (* ? *), [e]) ->
      (*
         ⟦raise e⟧ = λk ke. ⟦e⟧ ke ke
      *)
      let k = create_cont_ident "" in
      abs_cont k
        (continue_with
           (mkcont ~std:(Cid (err k)) k)
           (cps' e))

  | Lprim (prim, args) ->
      (*
         ⟦prim a⟧ = λk ke. ⟦a⟧ (λva. k (prim va)) ke

        A primitive call behave as a function, except that we do not
        have to evaluate the function term: once its arguments have
        been evaluated, we can directly call the primitive.
      *)
      let k = create_cont_ident "" in
      let args_cps = List.map cps' args in
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

        (where [e] is a free variable in [b])
      *)
      let k = create_cont_ident "" in
      abs_cont k
        (continue_with
           (mkcont
              ~err:(Clambda (exn, continue_with (mkcont k) (cps' handle)))
              k)
           (cps' body))

  | Lstaticcatch (body, (lbl, args), handle) ->
      (*
         ⟦catch a with lbl, args -> b⟧ = λk ke. ⟦λargs. b⟧ (λh. ⟦a⟧ k ke) ke
         + register (lbl → h) in [static_handlers]

        Translate the handler [λargs. b], and bind it to a fresh [h]
        in the body [a]. We also register the association (lbl → h) to
        the [static_handlers] table.

        As this is _static_ exceptions, whenever we translate [a] and
        encounter a [Lstaticraise (lbl, _)], we know we can handle it
        using [h].
      *)
      let k = create_cont_ident "" in
      let handler = Ident.create "handler" in
      static_handlers := (lbl, handler) :: !static_handlers;
      let args = if args = [] then [Ident.create "_"] else args in
      abs_cont k
        (continue_with
           (mkcont
              ~std:(Clambda (handler, continue_with (mkcont k) (cps' body)))
              k)
           (cps' (Lfunction { kind = Curried; params = args; body = handle })))

  | Lstaticraise (lbl, args) ->
      (*
         ⟦exit lbl args⟧ = ⟦h args⟧
         when (lbl → h) is in [static_handlers]

        As detailed for Lstaticcatch, as the code should be well
        scoped, a handler identifier should be registered in
        [static_handlers] for label [lbl]. We retrieve it and call it
        with the arguments of exit.
      *)
      let handler = List.assoc lbl !static_handlers in
      let args = if args = [] then [Lconst const_unit] else args in
      cps' (Lapply (Lvar handler, args, no_apply_info))

  | Lifthenelse (cond, thenbody, elsebody) ->
      (*
         ⟦if c then a else b⟧
         =
         λk ke. ⟦c⟧ (λvc. (if vc then ⟦a⟧ else ⟦b⟧) k ke) ke

        In [if c then a else b], [a]/[b] are only evaluated if [c]
        evaluates to [true]/[false]. Thus, in the translation, we
        select the right CPS translated [a]/[b] using if…then…else
        _before_ applying it to the continuations.
      *)
      let k = create_cont_ident "" in
      let condv = Ident.create "cond" in
      let body = continue_with (mkcont k)
          (Lifthenelse (
             Lvar condv,
             (cps' thenbody : lambda_cps :> lambda),
             (cps' elsebody : lambda_cps :> lambda)
           ) |> assert_cps)
      in

      abs_cont k
        (continue_with
           (mkcont ~std:(Clambda (condv, body)) k)
           (cps' cond))

  | Lswitch (t, { sw_numconsts;
                  sw_consts;
                  sw_numblocks;
                  sw_blocks;
                  sw_failaction }) ->
      (*
         ⟦switch a case c₁ -> b₁ | … | cₙ -> bₙ⟧
         =
         λk ke. ⟦a⟧ (λva. (switch va case c₁ -> ⟦b₁⟧ | … | cₙ -> ⟦bₙ⟧) k ke) ke

        In the same spirit as if…then…else, CPS-translate the cases, select
        the right one using a switch…case on the evaluated body, then continue
        with the continuation.
      *)
      let k = create_cont_ident "" in
      let tv = Ident.create "v" in
      let cps_case (i, t) = i, (cps' t : lambda_cps :> lambda) in
      let sw_consts = List.map cps_case sw_consts in
      let sw_blocks = List.map cps_case sw_blocks in
      let sw_failaction = match sw_failaction with
        | Some t -> Some (cps' t : lambda_cps :> lambda)
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
           (cps' t))

  | Lstringswitch (t, clauses, failaction) ->
      let k = create_cont_ident "" in
      let tv = Ident.create "v" in
      let cps_case (s, t) = s, (cps' t : lambda_cps :> lambda) in
      let failaction = match failaction with
        | Some t -> Some (cps' t : lambda_cps :> lambda)
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
           (cps' t))

  | Lwhile (cond, body) ->
    (*
       ⟦while cond do a done⟧
       =
       ⟦let rec whileloop () =
          if cond then
            (body; whileloop ())
          else
            ()
        in whileloop ()⟧

      Just translate the while loop to an equivalent recursive
      function, then CPS-translate this function.
    *)
    let loop = Ident.create "whileloop" in
    let p = Ident.create "param" in
    let loopdef = Lfunction {
      kind = Curried;
      params = [p];
      body = Lifthenelse (
        cond,
        Lsequence (body, Lapply (Lvar loop, [Lconst const_unit], no_apply_info)),
        Lconst const_unit
      )
    } in
    cps' (Lletrec ([loop, loopdef],
                   Lapply (Lvar loop,
                           [Lconst const_unit],
                           no_apply_info)))

  | Lfor (x, x_from, x_to, direction, body) ->
    (*
       ⟦for x = x_from to x_to do body done⟧
       =
       ⟦let rec forloop x =
          if x <= x_to then
            (body; forloop (x+1))
          else
            ()
        in forloop x_from⟧

      (similar translation for [downto] instead of [to])
      Translate the for loop to an equivalent recursive function, then
      CPS-translate it.
    *)
    let comp x y =
      match direction with
      | Asttypes.Upto -> Lprim (Pintcomp Cle, [x; y])
      | Asttypes.Downto -> Lprim (Pintcomp Cge, [x; y])
    in
    let step x =
      match direction with
      | Asttypes.Upto -> Lprim (Poffsetint 1, [x])
      | Asttypes.Downto -> Lprim (Poffsetint (-1), [x])
    in
    let loop = Ident.create "forloop" in
    let loopdef = Lfunction {
      kind = Curried;
      params = [x];
      body = Lifthenelse (
        comp (Lvar x) x_to,
        Lsequence (body, Lapply (Lvar loop, [step (Lvar x)], no_apply_info)),
        Lconst const_unit
      )
    } in
    cps' (Lletrec ([loop, loopdef],
                   Lapply (Lvar loop,
                           [x_from],
                           no_apply_info)))

  | Lassign (r, a) ->
    (* Let-bound references elimination is disabled. *)

    (* When generating the lambda code from the surface syntax, an
       optimization is generally performed for let-bound references
       that do not escape: instead of being allocated to the heap,
       they are allocated on the stack just like normal let-bound
       variables (using a let with kind Variable), and mutated using
       Lassign.

       ([eliminate_ref] from bytecomp/simplif.ml performs this
       optimization)

       However, in the context of a CPS-translation, this optimization
       does not make much sense, so instead of ahaving to unoptimize
       it to standard references, we disable it when the CPS
       translation is enabled.
    *)
    assert false

  | Lsend (kind, obj, meth, args, loc) ->
    let k = create_cont_ident "" in
    let objid = Ident.create "vo" in
    let methid = Ident.create "vm" in
    let args_id = List.map (fun _ -> Ident.create "v") args in
    let args_cps = List.map cps' args in
    let final_apply =
      Lapply (
        Lvar (std k),
        [Lsend (kind,
                Lvar objid,
                Lvar methid,
                List.map (fun i -> Lvar i) args_id, loc)],
        no_apply_info
      )
    in
    abs_cont k
      (cps_eval_chain k
         (List.combine (objid :: methid :: args_id)
            ((cps' obj) :: (cps' meth) :: args_cps))
         final_apply)

  | Levent (tm, ev) ->
    let k = create_cont_ident "" in
    let tmid = Ident.create "v" in
    abs_cont k
      (continue_with
         (mkcont ~std:(Clambda (tmid, Levent (Lvar tmid, ev))) k)
         (cps' tm))

  | Lifused (id, tm) ->
    let k = create_cont_ident "" in
    let tmid = Ident.create "v" in
    abs_cont k
      (continue_with
         (mkcont ~std:(Clambda (tmid, Lifused (id, Lvar tmid))) k)
         (cps' tm))

let cps (tm: lambda): lambda =
  ((cps (fun _ -> false) tm) :> lambda)

let toplevel_cps (tm: lambda): lambda =
  (* TODO: proper uncaught exception handler *)
  let x = Ident.create "x" in
  let identity = Lfunction { kind = Curried; params = [x]; body = Lvar x } in

  Lapply (
    (cps tm :> lambda),
    [identity; identity],
    no_apply_info
  )
