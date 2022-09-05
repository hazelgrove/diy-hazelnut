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

let rec consistent = (t1: htyp, t2: htyp): bool =>
  switch (t1, t2) {
  | (Hole, _)
  | (_, Hole) => true
  | (Arrow(t1, t2), Arrow(t1', t2')) =>
    consistent(t1, t2) && consistent(t1', t2')
  | (t1, t2) => t1 == t2
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
    if (ana(ctx, e2, t2)) {
      Some(t);
    } else {
      None;
    };
  | Num(_) => Some(Num)
  | Plus(e1, e2) =>
    if (ana(ctx, e1, Num) && ana(ctx, e2, Num)) {
      Some(Num);
    } else {
      None;
    }
  | Asc(e, t) =>
    if (ana(ctx, e, t)) {
      Some(t);
    } else {
      None;
    }
  | EHole => Some(Hole)
  | NEHole(e) =>
    let+ _ = syn(ctx, e);
    Hole;
  };
}
and ana = (ctx: typctx, e: hexp, t: htyp): bool => {
  switch (e) {
  | Lam(x, e) =>
    switch (matched_arrow_type(t)) {
    | Some((t1, t2)) =>
      let ctx' = TypCtx.add(x, t1, ctx);
      ana(ctx', e, t2);
    | None => false
    }
  | _ =>
    switch (syn(ctx, e)) {
    | Some(t') => consistent(t, t')
    | None => false
    }
  };
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
  switch (e, t, a) {
  // Movement: Asc
  | (Cursor(Asc(e1, t1)), _, Move(Child(One))) =>
    Some((LAsc(Cursor(e1), t1), t))
  | (Cursor(Asc(e1, t1)), _, Move(Child(Two))) =>
    Some((RAsc(e1, Cursor(t1)), t))
  | (LAsc(Cursor(e1), t1), _, Move(Parent))
  | (RAsc(e1, Cursor(t1)), _, Move(Parent)) =>
    Some((Cursor(Asc(e1, t1)), t))

  // Movement: Lam
  | (Cursor(Lam(x, e1)), _, Move(Child(One))) =>
    Some((Lam(x, Cursor(e1)), t))
  | (Lam(x, Cursor(e1)), _, Move(Parent)) =>
    Some((Cursor(Lam(x, e1)), t))

  // Movement: Plus
  | (Cursor(Plus(e1, e2)), _, Move(Child(One))) =>
    Some((LPlus(Cursor(e1), e2), t))
  | (Cursor(Plus(e1, e2)), _, Move(Child(Two))) =>
    Some((RPlus(e1, Cursor(e2)), t))
  | (LPlus(Cursor(e1), e2), _, Move(Parent))
  | (RPlus(e1, Cursor(e2)), _, Move(Parent)) =>
    Some((Cursor(Plus(e1, e2)), t))

  // Moveement: Ap
  | (Cursor(Ap(e1, e2)), _, Move(Child(One))) =>
    Some((LAp(Cursor(e1), e2), t))
  | (Cursor(Ap(e1, e2)), _, Move(Child(Two))) =>
    Some((RAp(e1, Cursor(e2)), t))
  | (LAp(Cursor(e1), e2), _, Move(Parent))
  | (RAp(e1, Cursor(e2)), _, Move(Parent)) =>
    Some((Cursor(Ap(e1, e2)), t))

  // Movement: NEHole
  | (Cursor(NEHole(e1)), _, Move(Child(One))) =>
    Some((NEHole(Cursor(e1)), t))
  | (NEHole(Cursor(e1)), _, Move(Parent)) =>
    Some((Cursor(NEHole(e1)), t))

  // Zipper Case: Ap
  | (LAp(ze, he), _, _) =>
    let* t2 = syn(ctx, erase_exp(ze));
    let* (ze', t3) = syn_action(ctx, (ze, t2), a);
    let* (t4, t5) = matched_arrow_type(t3);
    if (ana(ctx, he, t4)) {
      Some((LAp(ze', he), t5));
    } else {
      None;
    };
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
    (ze', ht);
  | (RAsc(he, zt), ht, _) =>
    if (ht == erase_typ(zt)) {
      let* zt' = typ_action(zt, a);
      let ht' = erase_typ(zt');
      if (ana(ctx, he, ht')) {
        Some((RAsc(he, zt'), ht'));
      } else {
        None;
      };
    } else {
      None;
    }

  // Zipper Case: NEHole
  | (NEHole(ze), Hole, _) =>
    let* ht = syn(ctx, erase_exp(ze));
    let+ (ze', _) = syn_action(ctx, (ze, ht), a);
    (NEHole(ze'): zexp, Hole);

  | (_, _, Construct(_) | Del | Finish) => raise(Unimplemented)

  | _ => None
  };
}
and ana_action = (ctx: typctx, e: zexp, a: action, t: htyp): option(zexp) => {
  switch (e, a, t) {
  // Zipper Case: Lam
  | (Lam(x, ze), _, _) =>
    let* (t1, t2) = matched_arrow_type(t);
    let ctx' = TypCtx.add(x, t1, ctx);
    let+ ze' = ana_action(ctx', ze, a, t2);
    (Lam(x, ze'): zexp);

  // Subsumption
  | (_, _, _) =>
    let* t' = syn(ctx, erase_exp(e));
    let* (e', t'') = syn_action(ctx, (e, t'), a);
    if (consistent(t, t'')) {
      Some(e');
    } else {
      None;
    };
  };
};
