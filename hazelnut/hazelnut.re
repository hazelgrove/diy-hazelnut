open Sexplib.Std;
open Monad_lib.Monad; // Uncomment this line to use the maybe monad

let compare_string = String.compare;
let compare_int = Int.compare;
let compare_bool = Bool.compare;

module Htyp = {
  [@deriving (sexp, compare)]
  type t =
    | Arrow(t, t)
    | Num
    | Hole;
};

module Ztyp = {
  [@deriving (sexp, compare)]
  type t =
    | Cursor(Htyp.t)
    | LArrow(t, Htyp.t)
    | RArrow(Htyp.t, t);
};

module Mark = {
  [@deriving (sexp, compare)]
  type t =
    | Free
    | NonArrowAp
    | LamNonArrow
    | Inconsistent;
};

module Hexp = {
  [@deriving (sexp, compare)]
  type t =
    | Var(string) // x
    | NumLit(int) // n
    | Plus(t, t) // e + e
    | Lam(string, Htyp.t, t) // \x:t.e
    | Ap(t, t) // e e
    | Asc(t, Htyp.t) // e : t
    | EHole // hole
    | Mark(t, Mark.t); // mark
};

module Zexp = {
  [@deriving (sexp, compare)]
  type t =
    | Cursor(Hexp.t)
    | LPlus(t, Hexp.t)
    | RPlus(Hexp.t, t)
    | LLam(string, Ztyp.t, Hexp.t)
    | RLam(string, Htyp.t, t)
    | LAsc(t, Htyp.t)
    | RAsc(Hexp.t, Ztyp.t)
    | LAp(t, Hexp.t)
    | RAp(Hexp.t, t)
    | Mark(t, Mark.t);
};

module Child = {
  [@deriving (sexp, compare)]
  type t =
    | One
    | Two
    | Three;
};

module Dir = {
  [@deriving (sexp, compare)]
  type t =
    | Child(Child.t)
    | Parent;
};

module Shape = {
  [@deriving (sexp, compare)]
  type t =
    | Arrow
    | Num
    | Var(string)
    | NumLit(int)
    | Plus
    | Lam(string)
    | Asc
    | Ap;
};

module Action = {
  [@deriving (sexp, compare)]
  type t =
    | Move(Dir.t)
    | Construct(Shape.t)
    | Del;
};

module TypCtx = Map.Make(String);
type typctx = TypCtx.t(Htyp.t);

exception Unimplemented;

// Perform cursor erasure
let rec erase_typ = (t: Ztyp.t): Htyp.t => {
  switch (t) {
  | Cursor(t) => t
  | LArrow(zt1, t2) => Arrow(erase_typ(zt1), t2)
  | RArrow(t1, zt2) => Arrow(t1, erase_typ(zt2))
  };
};

let rec erase_exp = (e: Zexp.t): Hexp.t => {
  switch (e) {
  | Cursor(e') => e'
  | LLam(name, asc, body) => Lam(name, erase_typ(asc), body)
  | RLam(name, asc, body) => Lam(name, asc, erase_exp(body))
  | LAsc(e, t) => Asc(erase_exp(e), t)
  | RAsc(e, t) => Asc(e, erase_typ(t))
  | LAp(zt, et) => Ap(erase_exp(zt), et)
  | RAp(et, zt) => Ap(et, erase_exp(zt))
  | LPlus(zt, et) => Plus(erase_exp(zt), et)
  | RPlus(et, zt) => Plus(et, erase_exp(zt))
  | Mark(e, m) => Mark(erase_exp(e), m)
  };
};

// Mark erasure
let rec mark_erase_exp = (e: Hexp.t): Hexp.t => {
  switch (e) {
  | Cursor(e') => e'
  | LLam(name, asc, body) => Lam(name, erase_typ(asc), body)
  | RLam(name, asc, body) => Lam(name, asc, erase_exp(body))
  | LAsc(e, t) => Asc(erase_exp(e), t)
  | RAsc(e, t) => Asc(e, erase_typ(t))
  | LAp(zt, et) => Ap(erase_exp(zt), et)
  | RAp(et, zt) => Ap(et, erase_exp(zt))
  | LPlus(zt, et) => Plus(erase_exp(zt), et)
  | RPlus(et, zt) => Plus(et, erase_exp(zt))
  | Mark(e, m) => Mark(erase_exp(e), m)
  };
};

let matched_arrow_typ = (t: Htyp.t): option((Htyp.t, Htyp.t)) => {
  switch (t) {
  | Arrow(t1, t2) => Some((t1, t2))
  | Hole => Some((Hole, Hole))
  | _ => None
  };
};

let rec type_meet = (t1: Htyp.t, t2: Htyp.t): option(Htyp.t) => {
  switch (t1, t2) {
  | (Hole, _) => Some(t2)
  | (_, Hole) => Some(t1)
  | (Num, Num) => Some(Num)
  | (Arrow(t11, t12), Arrow(t21, t22)) =>
    let* t1 = type_meet(t11, t21);
    let* t2 = type_meet(t12, t22);
    Some(Htyp.Arrow(t1, t2));
  | _ => None
  };
};

let rec type_consistent = (t1: Htyp.t, t2: Htyp.t): bool => {
  switch (t1, t2) {
  | (Hole, _) => true
  | (_, Hole) => true
  | (Num, Num) => true
  | (Arrow(t11, t12), Arrow(t21, t22)) =>
    type_consistent(t11, t21) && type_consistent(t12, t22)
  | _ => false
  };
};

// let rec syn = (ctx: typctx, e: Hexp.t): option(Htyp.t) => {
//   switch (e) {
//   | Var(name) => TypCtx.find_opt(name, ctx)
//   | Lam(arg_name, type_asc, body) =>
//     let* t2 = syn(TypCtx.add(arg_name, type_asc, ctx), body);
//     Some(Htyp.Arrow(type_asc, t2));
//   | Ap(func, arg) =>
//     let* t1 = syn(ctx, func);
//     let* (t2, t) = matched_arrow_typ(t1);
//     if (ana(ctx, arg, t2)) {
//       Some(t);
//     } else {
//       None;
//     };
//   | Asc(e, t) =>
//     if (ana(ctx, e, t)) {
//       Some(t);
//     } else {
//       None;
//     }
//   | NumLit(_) => Some(Htyp.Num)
//   | Plus(arg1, arg2) =>
//     let e1_ana = ana(ctx, arg1, Htyp.Num);
//     let e2_ana = ana(ctx, arg2, Htyp.Num);
//     if (e1_ana && e2_ana) {
//       Some(Htyp.Num);
//     } else {
//       None;
//     };
//   | EHole => Some(Htyp.Hole)
//   | Mark(exp, _) =>
//     switch (syn(ctx, exp)) {
//     | Some(_) => Some(Htyp.Hole)
//     | None => None
//     }
//   };
// }
// and ana = (ctx: typctx, e: Hexp.t, t: Htyp.t): bool => {
//   switch (e) {
//   | Lam(var_name, asc, body) =>
//     let a = matched_arrow_typ(t);
//     switch (a) {
//     | Some((t1, t2)) =>
//       if (type_consistent(t1, asc)) {
//         ana(TypCtx.add(var_name, asc, ctx), body, t2);
//       } else {
//         false;
//       }
//     | _ => false
//     };
//   | _ =>
//     switch (syn(ctx, e)) {
//     | Some(t') => type_consistent(t, t')
//     | None => false
//     }
//   };
// };

let rec move_typ = (t: Ztyp.t, a: Dir.t): Ztyp.t => {
  switch (t) {
  | Cursor(typ) =>
    switch (typ, a) {
    | (Arrow(t1, t2), Child(One)) => LArrow(Cursor(t1), t2)
    | (Arrow(t1, t2), Child(Two)) => RArrow(t1, Cursor(t2))
    | _ => raise(Unimplemented)
    }
  | LArrow(zt1, t2) =>
    switch (a) {
    | Child(_) => LArrow(move_typ(zt1, a), t2)
    | Parent =>
      switch (zt1) {
      | Cursor(t1) => Cursor(Arrow(t1, t2))
      | _ => LArrow(move_typ(zt1, a), t2)
      }
    }
  | RArrow(t1, zt2) =>
    switch (a) {
    | Child(_) => RArrow(t1, move_typ(zt2, a))
    | Parent =>
      switch (zt2) {
      | Cursor(t2) => Cursor(Arrow(t1, t2))
      | _ => RArrow(t1, move_typ(zt2, a))
      }
    }
  };
};

let rec move_action = (e: Zexp.t, a: Dir.t): Zexp.t => {
  switch (e) {
  | Cursor(e') =>
    // child movements here
    switch (e', a) {
    | (Lam(name, asc, body), Child(One)) => LLam(name, Cursor(asc), body)
    | (Lam(name, asc, body), Child(Two)) => RLam(name, asc, Cursor(body))
    | (Asc(e, t), Child(One)) => LAsc(Cursor(e), t)
    | (Asc(e, t), Child(Two)) => RAsc(e, Cursor(t))
    | (Ap(e1, e2), Child(One)) => LAp(Cursor(e1), e2)
    | (Ap(e1, e2), Child(Two)) => RAp(e1, Cursor(e2))
    | (Plus(e1, e2), Child(One)) => LPlus(Cursor(e1), e2)
    | (Plus(e1, e2), Child(Two)) => RPlus(e1, Cursor(e2))
    | (Mark(e, m), Child(One)) => Mark(Cursor(e), m)
    | _ => raise(Unimplemented)
    }
  | LLam(name, asc, body) =>
    switch (a, asc) {
    | (Parent, Cursor(typ)) => Cursor(Lam(name, typ, body))
    | (_, _) => LLam(name, move_typ(asc, a), body)
    }
  | RLam(name, asc, body) =>
    switch (a, body) {
    | (Parent, Cursor(body)) => Cursor(Lam(name, asc, body))
    | (_, _) => RLam(name, asc, move_action(body, a))
    }
  | LAsc(e, t) =>
    switch (a, e) {
    | (Parent, Cursor(e)) => Cursor(Asc(e, t))
    | (_, _) => LAsc(move_action(e, a), t)
    }
  | RAsc(e, t) =>
    switch (a, t) {
    | (Parent, Cursor(t)) => Cursor(Asc(e, t))
    | (_, _) => RAsc(e, move_typ(t, a))
    }
  | LAp(zt, et) =>
    switch (a, zt) {
    | (Parent, Cursor(t)) => Cursor(Ap(t, et))
    | (_, _) => LAp(move_action(zt, a), et)
    }
  | RAp(et, zt) =>
    switch (a, zt) {
    | (Parent, Cursor(t)) => Cursor(Ap(et, t))
    | (_, _) => RAp(et, move_action(zt, a))
    }
  | LPlus(zt, et) =>
    switch (a, zt) {
    | (Parent, Cursor(t)) => Cursor(Plus(t, et))
    | (_, _) => LPlus(move_action(zt, a), et)
    }
  | RPlus(et, zt) =>
    switch (a, zt) {
    | (Parent, Cursor(t)) => Cursor(Plus(et, t))
    | (_, _) => RPlus(et, move_action(zt, a))
    }
  | Mark(ze, m) =>
    switch (a, ze) {
    | (Parent, Cursor(t)) => Cursor(Mark(t, m))
    | (_, _) => Mark(move_action(ze, a), m)
    }
  };
};

let rec typ_action = (t: Ztyp.t, a: Action.t): Ztyp.t => {
  switch (a) {
  | Move(dir) => move_typ(t, dir)
  | _ =>
    switch (t) {
    | Cursor(typ) =>
      switch (a) {
      | Construct(shape) =>
        switch (typ, shape) {
        | (Hole, Num) => Cursor(Num)
        | (sub_t, Arrow) => Cursor(Arrow(sub_t, Hole))
        | _ => t
        }
      | Del => Cursor(Hole)
      | Move(_) => t // this cant happen
      }
    | LArrow(zt, t) =>
      let zt' = typ_action(zt, a);
      LArrow(zt', t);
    | RArrow(t, zt) =>
      let zt' = typ_action(zt, a);
      RArrow(t, zt');
    }
  };
};
let rec exp_action = (e: Zexp.t, a: Action.t): Zexp.t => {
  switch (a) {
  | Move(dir) => move_action(e, dir)
  | _ =>
    switch (e) {
    | Cursor(h_exp) =>
      switch (a) {
      | Move(_) => failwith("impossible")
      | Construct(Arrow)
      | Construct(Num) => e
      | Construct(Var(name)) =>
        switch (h_exp) {
        | EHole => Cursor(Var(name))
        | _ => e // cant construct a var on a non-hole
        }
      | Construct(Lam(name)) =>
        switch (h_exp) {
        | EHole => LLam(name, Cursor(Hole), EHole)
        | _ => e
        }
      | Construct(Asc) => RAsc(h_exp, Cursor(Hole))
      | Construct(Ap) => RAp(h_exp, Cursor(EHole))
      | Construct(NumLit(n)) =>
        switch (h_exp) {
        | EHole => Cursor(NumLit(n))
        | _ => e
        }
      | Construct(Plus) => RPlus(h_exp, Cursor(EHole))
      | Del => Cursor(EHole)
      }
    // zipper cases
    | LAp(z_exp, h_exp) => LAp(exp_action(z_exp, a), h_exp)
    | RAp(h_exp, z_exp) => RAp(h_exp, exp_action(z_exp, a))
    | LLam(_, _, _) => e
    | RLam(_, _, _) => e
    | LPlus(z_exp, h_exp) => LPlus(exp_action(z_exp, a), h_exp)
    | RPlus(h_exp, z_exp) => RPlus(h_exp, exp_action(z_exp, a))
    | LAsc(z_exp, h_typ) => LAsc(exp_action(z_exp, a), h_typ)
    | RAsc(h_exp, z_typ) => RAsc(h_exp, typ_action(z_typ, a))
    | Mark(_) => failwith("impossible")
    }
  };
};

let rec mark_syn = (ctx: typctx, e: Hexp.t): (Hexp.t, Htyp.t) => {
  switch (e) {
  | Var(x) =>
    switch (TypCtx.find_opt(x, ctx)) {
    | None => (Mark(e, Free), Hole)
    | Some(t) => (e, t)
    }
  | NumLit(_) => (e, Num)
  | EHole => (e, Hole)
  | Lam(x, t, e) =>
    let (e', t') = mark_syn(TypCtx.add(x, t, ctx), e);
    (Lam(x, t, e'), Arrow(t, t'));
  | Ap(e1, e2) =>
    let (e1', t1) = mark_syn(ctx, e1);
    switch (matched_arrow_typ(t1)) {
    | None =>
      let e2' = mark_ana(ctx, Hole, e2);
      (Ap(Mark(e1', NonArrowAp), e2'), Hole);
    | Some((t11, t12)) =>
      let e2' = mark_ana(ctx, t11, e2);
      (Ap(e1', e2'), t12);
    };
  | Plus(e1, e2) => (
      Plus(mark_ana(ctx, Num, e1), mark_ana(ctx, Num, e2)),
      Num,
    )
  | Asc(e, t) => (Asc(mark_ana(ctx, t, e), t), t)
  | Mark(_) => failwith("impossible")
  };
}

and mark_ana = (ctx: typctx, t: Htyp.t, e: Hexp.t): Hexp.t => {
  let subsume = (e): Hexp.t => {
    let (e', t') = mark_syn(ctx, e);
    if (type_consistent(t, t')) {
      e';
    } else {
      Mark(e', Inconsistent);
    };
  };
  switch (e) {
  | Lam(x, t', body) =>
    switch (matched_arrow_typ(t)) {
    | None => subsume(e)
    | Some((t1, t2)) =>
      let body' = mark_ana(TypCtx.add(x, t', ctx), t2, body);
      if (type_consistent(t1, t')) {
        Lam(x, t', body');
      } else {
        Mark(Lam(x, t', body'), LamNonArrow);
      };
    }
  | _ => subsume(e)
  };
};
