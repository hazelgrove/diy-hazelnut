open Sexplib.Std;

// TODO: Figure out how to get compare_string and compare_int to exist without manually defining them
let compare_string = String.compare;
let compare_int = Int.compare;

[@deriving (sexp, compare)]
type htyp =
  | Arrow(htyp, htyp)
  | Num
  | Hole;

[@deriving (sexp, compare)]
type hexp =
  | Var(string)
  | Lam(string, hexp)
  | Ap(hexp, hexp)
  | Num(int)
  | Plus(hexp, hexp)
  | Asc(hexp, htyp)
  | EHole
  | NEHole(hexp);

[@deriving (sexp, compare)]
type ztyp =
  | Cursor(htyp)
  | LArrow(ztyp, htyp)
  | RArrow(htyp, ztyp);

[@deriving (sexp, compare)]
type zexp =
  | Cursor(hexp)
  | Lam(string, zexp)
  | LAp(zexp, hexp)
  | RAp(hexp, zexp)
  | LPlus(zexp, hexp)
  | RPlus(hexp, zexp)
  | LAsc(zexp, htyp)
  | RAsc(hexp, ztyp)
  | NEHole(zexp);

[@deriving (sexp, compare)]
type child =
  | One
  | Two;

[@deriving (sexp, compare)]
type dir =
  | Child(child)
  | Parent;

[@deriving (sexp, compare)]
type shape =
  | Arrow
  | Num
  | Asc
  | Var(string)
  | Lam(string)
  | Ap
  | Lit(int)
  | Plus
  | NEHole;

[@deriving (sexp, compare)]
type action =
  | Move(dir)
  | Construct(shape)
  | Del
  | Finish;

module TypCtx = Map.Make(String);
type typctx = TypCtx.t(htyp);

exception Unimplemented;

// Maybe Monad
let ( let* ) = (x: option('a), f: 'a => option('b)): option('b) =>
  switch (x) {
  | Some(x) => f(x)
  | None => None
  };

// Maybe Monad
let (let+) = (x: option('a), f: 'a => 'b): option('b) => {
  let* x = x;
  Some(f(x));
};

let matched_arrow_type: htyp => option((htyp, htyp)) =
  fun
  | Hole => Some((Hole, Hole))
  | Arrow(t1, t2) => Some((t1, t2))
  | _ => None;

let rec erase_typ: ztyp => htyp =
  fun
  | Cursor(t) => t
  | LArrow(t1, t2) => Arrow(erase_typ(t1), t2)
  | RArrow(t1, t2) => Arrow(t1, erase_typ(t2));

let rec erase_exp: zexp => hexp =
  fun
  | Cursor(e) => e
  | Lam(x, e) => Lam(x, erase_exp(e))
  | LAp(e1, e2) => Ap(erase_exp(e1), e2)
  | RAp(e1, e2) => Ap(e1, erase_exp(e2))
  | LPlus(e1, e2) => Plus(erase_exp(e1), e2)
  | RPlus(e1, e2) => Plus(e1, erase_exp(e2))
  | LAsc(e, t) => Asc(erase_exp(e), t)
  | RAsc(e, t) => Asc(e, erase_typ(t))
  | NEHole(e) => NEHole(erase_exp(e));

let rec consistent = (t1: htyp, t2: htyp): option(unit) =>
  switch (t1, t2) {
  | (Hole, _)
  | (_, Hole) => Some()
  | (Arrow(t1, t2), Arrow(t1', t2')) =>
    let* () = consistent(t1, t2);
    let+ () = consistent(t1', t2');
    ();
  | (t1, t2) =>
    if (t1 == t2) {
      Some();
    } else {
      None;
    }
  };

let inconsistent = (t1: htyp, t2: htyp): option(unit) =>
  switch (consistent(t1, t2)) {
  | Some () => None
  | None => Some()
  };

let rec syn = (ctx: typctx, e: hexp): option(htyp) => {
  switch (e) {
  | Var(x) =>
    try(Some(TypCtx.find(x, ctx))) {
    | Not_found => None
    }
  | Lam(_, _) => None
  | Ap(e1, e2) =>
    let* t1 = syn(ctx, e1);
    let* (t2, t) = matched_arrow_type(t1);
    let+ () = ana(ctx, e2, t2);
    t;
  | Num(_) => Some(Num)
  | Plus(e1, e2) =>
    let* () = ana(ctx, e1, Num);
    let+ () = ana(ctx, e2, Num);
    (Num: htyp);
  | Asc(e, t) =>
    let+ () = ana(ctx, e, t);
    t;
  | EHole => Some(Hole)
  | NEHole(e) =>
    let+ _ = syn(ctx, e);
    Hole;
  };
}

and ana = (ctx: typctx, e: hexp, t: htyp): option(unit) => {
  switch (e) {
  | Lam(x, e) =>
    let* (t1, t2) = matched_arrow_type(t);
    let ctx' = TypCtx.add(x, t1, ctx);
    ana(ctx', e, t2);
  | _ =>
    let* t' = syn(ctx, e);
    consistent(t, t');
  };
};

let exp_movement = (e: zexp, d: dir): option(zexp) =>
  switch (e, d) {
  | (Cursor(Asc(e, t)), Child(One)) => Some(LAsc(Cursor(e), t))
  | (Cursor(Asc(e, t)), Child(Two)) => Some(RAsc(e, Cursor(t)))
  | (LAsc(Cursor(e), t), Parent)
  | (RAsc(e, Cursor(t)), Parent) => Some(Cursor(Asc(e, t)))

  | (Cursor(Lam(x, e)), Child(One)) => Some(Lam(x, Cursor(e)))
  | (Lam(x, Cursor(e)), Parent) => Some(Cursor(Lam(x, e)))

  | (Cursor(Plus(e1, e2)), Child(One)) => Some(LPlus(Cursor(e1), e2))
  | (Cursor(Plus(e1, e2)), Child(Two)) => Some(RPlus(e1, Cursor(e2)))
  | (LPlus(Cursor(e1), e2), Parent)
  | (RPlus(e1, Cursor(e2)), Parent) => Some(Cursor(Plus(e1, e2)))

  | (Cursor(Ap(e1, e2)), Child(One)) => Some(LAp(Cursor(e1), e2))
  | (Cursor(Ap(e1, e2)), Child(Two)) => Some(RAp(e1, Cursor(e2)))
  | (LAp(Cursor(e1), e2), Parent)
  | (RAp(e1, Cursor(e2)), Parent) => Some(Cursor(Ap(e1, e2)))

  | (Cursor(NEHole(e)), Child(One)) => Some(NEHole(Cursor(e)))
  | (NEHole(Cursor(e)), Parent) => Some(Cursor(NEHole(e)))

  | _ => None
  };

let rec typ_action = (t: ztyp, a: action): option(ztyp) => {
  switch (t, a) {
  // Movement: Arrow
  | (Cursor(Arrow(t1, t2)), Move(Child(One))) =>
    Some(LArrow(Cursor(t1), t2))
  | (Cursor(Arrow(t1, t2)), Move(Child(Two))) =>
    Some(RArrow(t1, Cursor(t2)))
  | (LArrow(Cursor(t1), t2), Move(Parent))
  | (RArrow(t1, Cursor(t2)), Move(Parent)) => Some(Cursor(Arrow(t1, t2)))

  // Deletion
  | (_, Del) => Some(Cursor(Hole))

  // Construction
  | (Cursor(t), Construct(Arrow)) => Some(RArrow(t, Cursor(Hole)))
  | (Cursor(Hole), Construct(Num)) => Some(Cursor(Num))

  // Zipper Case: Arrow
  | (LArrow(zt, ht), a) =>
    let+ zt' = typ_action(zt, a);
    LArrow(zt', ht);
  | (RArrow(ht, zt), a) =>
    let+ zt' = typ_action(zt, a);
    RArrow(ht, zt');

  | _ => None
  };
};

let rec syn_action =
        (ctx: typctx, (e: zexp, t: htyp), a: action): option((zexp, htyp)) => {
  // Try movement first
  let movement =
    switch (a) {
    | Move(d) => exp_movement(e, d)
    | _ => None
    };

  // If movement fails, try synthetic actions
  switch (movement) {
  | Some(e') => Some((e', t))
  | None =>
    switch (e, t, a) {
    // Construct: Asc
    | (Cursor(e1), _, Construct(Asc)) => Some((RAsc(e1, Cursor(t)), t))

    // Construct: Var
    | (Cursor(EHole), Hole, Construct(Var(x))) =>
      try({
        let ht = TypCtx.find(x, ctx);
        Some((Cursor(Var(x)), ht));
      }) {
      | Not_found => None
      }

    // Construct: Lam
    | (Cursor(EHole), Hole, Construct(Lam(x))) =>
      Some((
        RAsc(Lam(x, EHole), LArrow(Cursor(Hole), Hole)),
        Arrow(Hole, Hole),
      ))

    // Construct: Ap
    | (Cursor(he), _, Construct(Ap)) =>
      switch (matched_arrow_type(t), inconsistent(t, Arrow(Hole, Hole))) {
      | (Some((_, t2)), None) => Some((RAp(he, Cursor(EHole)), t2))
      | (None, Some ()) => Some((RAp(NEHole(he), Cursor(EHole)), Hole))
      | _ => None
      }

    // Construct: Num
    | (Cursor(EHole), Hole, Construct(Lit(n))) =>
      Some((Cursor(Num(n)), Num))

    // Construct: Plus
    | (Cursor(he), _, Construct(Plus)) =>
      switch (consistent(t, Num)) {
      | Some () => Some((RPlus(he, Cursor(EHole)), Num))
      | None => Some((RPlus(NEHole(he), Cursor(EHole)), Num))
      }

    // Construct: NEHole
    | (Cursor(he), _, Construct(NEHole)) =>
      Some((NEHole(Cursor(he)), Hole))

    // Zipper Case: Ap
    | (LAp(ze, he), _, _) =>
      let* t2 = syn(ctx, erase_exp(ze));
      let* (ze', t3) = syn_action(ctx, (ze, t2), a);
      let* (t4, t5) = matched_arrow_type(t3);
      let+ () = ana(ctx, he, t4);
      (LAp(ze', he), t5);
    | (RAp(he, ze), _, _) =>
      let* t2 = syn(ctx, he);
      let* (t3, t4) = matched_arrow_type(t2);
      let+ ze' = ana_action(ctx, ze, a, t3);
      (RAp(he, ze'), t4);

    // Zipper Case: Plus
    | (LPlus(ze, he), Num, _) =>
      let+ ze' = ana_action(ctx, ze, a, Num);
      (LPlus(ze', he), Num: htyp);
    | (RPlus(he, ze), Num, _) =>
      let+ ze' = ana_action(ctx, ze, a, Num);
      (RPlus(he, ze'), Num: htyp);

    // Zipper Case: Asc
    | (LAsc(ze, ht), _, _) =>
      let+ ze' = ana_action(ctx, ze, a, ht);
      (LAsc(ze', ht), ht);
    | (RAsc(he, zt), ht, _) =>
      if (ht == erase_typ(zt)) {
        let* zt' = typ_action(zt, a);
        let ht' = erase_typ(zt');
        let+ () = ana(ctx, he, ht');
        (RAsc(he, zt'), ht');
      } else {
        None;
      }

    // Zipper Case: NEHole
    | (NEHole(ze), Hole, _) =>
      let* ht = syn(ctx, erase_exp(ze));
      let+ (ze', _) = syn_action(ctx, (ze, ht), a);
      (NEHole(ze'): zexp, Hole);

    | (_, _, | Del | Finish) => raise(Unimplemented)

    | _ => None
    }
  };
}

and ana_action = (ctx: typctx, e: zexp, a: action, t: htyp): option(zexp) => {
  // Try movement first
  let movement =
    switch (a) {
    | Move(d) => exp_movement(e, d)
    | _ => None
    };

  // If movement fails, try analytic actions
  let analysis =
    switch (movement) {
    | Some(_) => movement
    | None =>
      switch (e, a, t) {
      // Construct: Asc
      | (Cursor(e1), Construct(Asc), _) => Some(RAsc(e1, Cursor(t)))

      // Construct: Var
      | (Cursor(EHole), Construct(Var(x)), _) =>
        try({
          let t' = TypCtx.find(x, ctx);
          let+ () = inconsistent(t, t');
          (NEHole(Cursor(Var(x))): zexp);
        }) {
        | Not_found => None
        }

      // Construct: Lam
      | (Cursor(EHole), Construct(Lam(x)), _) =>
        switch (matched_arrow_type(t), inconsistent(t, Arrow(Hole, Hole))) {
        | (Some(_), None) => Some(Lam(x, Cursor(EHole)))
        | (None, Some ()) =>
          Some(NEHole(RAsc(Lam(x, EHole), LArrow(Cursor(Hole), Hole))))
        | _ => None
        }

      // Construct: Num
      | (Cursor(EHole), Construct(Lit(n)), _) =>
        let+ () = inconsistent(t, Num);
        (NEHole(Cursor(Num(n))): zexp);

      // Zipper Case: Lam
      | (Lam(x, ze), _, _) =>
        let* (t1, t2) = matched_arrow_type(t);
        let ctx' = TypCtx.add(x, t1, ctx);
        let+ ze' = ana_action(ctx', ze, a, t2);
        (Lam(x, ze'): zexp);

      | _ => None
      }
    };

  // If analysis fails, try subsumption as a last resort
  switch (analysis) {
  | Some(_) => analysis
  | None =>
    let* t' = syn(ctx, erase_exp(e));
    let* (e', t'') = syn_action(ctx, (e, t'), a);
    let+ () = consistent(t, t'');
    e';
  };
};
