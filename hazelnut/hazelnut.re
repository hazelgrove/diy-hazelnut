open Sexplib.Std;
open Monad_lib.Monad; // Uncomment this line to use the maybe monad

let compare_string = String.compare;
let compare_int = Int.compare;
let compare_bool = Bool.compare;

module Htyp = {
  [@deriving (sexp, compare)]
  type t =
    | Arrow(t, t)
    | Product(t, t)
    | Num
    | Bool
    | Hole;
};

module Ztyp = {
  [@deriving (sexp, compare)]
  type t =
    | Cursor(Htyp.t)
    | LArrow(t, Htyp.t)
    | RArrow(Htyp.t, t)
    | LProd(t, Htyp.t)
    | RProd(Htyp.t, t);
};

module Mark = {
  [@deriving (sexp, compare)]
  type t =
    | InconsistentTypes
    | InconsistentBranches
    | InconsistentAscription
    | Free
    | UnexpectedType(Htyp.t);
};

module Hexp = {
  [@deriving (sexp, compare)]
  type t =
    | Var(string) // x
    | Lam(string, Htyp.t, t) // \x:t.e
    | Ap(t, t) // e e
    | Let(string, t, t) // let x = e in e
    | Asc(t, Htyp.t) // e : t
    | NumLit(int) // n
    | BoolLit(bool) // b
    | Prod(t, t) // (e, e)
    | Plus(t, t) // e + e
    | Cond(t, t, t) // if e then e else e
    | Proj1(t) // e.1
    | Proj2(t) // e.2
    | EHole // hole
    | MarkHole(t, Mark.t); // mark hole
};

module Zexp = {
  [@deriving (sexp, compare)]
  type t =
    | Cursor(Hexp.t)
    | LLam(string, Ztyp.t, Hexp.t)
    | RLam(string, Htyp.t, t)
    | LLet(string, t, Hexp.t)
    | RLet(string, Hexp.t, t)
    | LAsc(t, Htyp.t)
    | RAsc(Hexp.t, Ztyp.t)
    | LProd(t, Hexp.t)
    | RProd(Hexp.t, t)
    | Proj1(t)
    | Proj2(t)
    | LAp(t, Hexp.t)
    | RAp(Hexp.t, t)
    | LPlus(t, Hexp.t)
    | RPlus(Hexp.t, t)
    | IfCond(t, Hexp.t, Hexp.t)
    | ThenCond(Hexp.t, t, Hexp.t)
    | ElseCond(Hexp.t, Hexp.t, t)
    | MarkHole(t, Mark.t);
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
    | Bool
    | ProdT
    | Var(string)
    | Lam(string)
    | Let(string)
    | Asc
    | Ap
    | NumLit(int)
    | BoolLit(bool)
    | Cond
    | Proj1
    | Proj2
    | Prod
    | Plus
    | NEHole;
};

module Action = {
  [@deriving (sexp, compare)]
  type t =
    | Move(Dir.t)
    | Construct(Shape.t)
    | Del
    | Finish;
};

module TypCtx = Map.Make(String);
type typctx = TypCtx.t(Htyp.t);

exception Unimplemented;

// Perform cursor erasure on a Zexp.t
let rec erase_exp = (e: Zexp.t): Hexp.t => {
  switch (e) {
  | Cursor(e') => e'
  | LLam(name, asc, body) => Lam(name, erase_typ(asc), body)
  | RLam(name, asc, body) => Lam(name, asc, erase_exp(body))
  | LLet(name, e1, e2) => Let(name, erase_exp(e1), e2)
  | RLet(name, e1, e2) => Let(name, e1, erase_exp(e2))
  | LAsc(e, t) => Asc(erase_exp(e), t)
  | RAsc(e, t) => Asc(e, erase_typ(t))
  | LProd(e1, e2) => Prod(erase_exp(e1), e2)
  | RProd(e1, e2) => Prod(e1, erase_exp(e2))
  | Proj1(e) => Proj1(erase_exp(e))
  | Proj2(e) => Proj2(erase_exp(e))
  | LAp(zt, et) => Ap(erase_exp(zt), et)
  | RAp(et, zt) => Ap(et, erase_exp(zt))
  | LPlus(zt, et) => Plus(erase_exp(zt), et)
  | RPlus(et, zt) => Plus(et, erase_exp(zt))
  | IfCond(zp, ht, he) => Cond(erase_exp(zp), ht, he)
  | ThenCond(p, zt, he) => Cond(p, erase_exp(zt), he)
  | ElseCond(p, ht, ze) => Cond(p, ht, erase_exp(ze))
  | MarkHole(e, m) => MarkHole(erase_exp(e), m)
  };
}
and erase_typ = (t: Ztyp.t): Htyp.t => {
  // Used to suppress unused variable warnings
  // Okay to remove
  switch (t) {
  | Cursor(t) => t
  | LArrow(zt1, t2) => Arrow(erase_typ(zt1), t2)
  | RArrow(t1, zt2) => Arrow(t1, erase_typ(zt2))
  | LProd(zt1, t2) => Product(erase_typ(zt1), t2)
  | RProd(t1, zt2) => Product(t1, erase_typ(zt2))
  };
};

let matched_arrow_typ = (t: Htyp.t): option((Htyp.t, Htyp.t)) => {
  switch (t) {
  | Arrow(t1, t2) => Some((t1, t2))
  | Hole => Some((Hole, Hole))
  | _ => None
  };
};

let matched_product_typ = (t: Htyp.t): option((Htyp.t, Htyp.t)) => {
  switch (t) {
  | Product(t1, t2) => Some((t1, t2))
  | Hole => Some((Hole, Hole))
  | _ => None
  };
};

let rec type_meet = (t1: Htyp.t, t2: Htyp.t): option(Htyp.t) => {
  switch (t1, t2) {
  | (Hole, _) => Some(t2)
  | (_, Hole) => Some(t1)
  | (Num, Num) => Some(Num)
  | (Bool, Bool) => Some(Bool)
  | (Arrow(t11, t12), Arrow(t21, t22)) =>
    let* t1 = type_meet(t11, t21);
    let* t2 = type_meet(t12, t22);
    Some(Htyp.Arrow(t1, t2));
  | (Product(t11, t12), Product(t21, t22)) =>
    let* t1 = type_meet(t11, t21);
    let* t2 = type_meet(t12, t22);
    Some(Htyp.Product(t1, t2));
  | _ => None
  };
};

let rec type_consistent = (t1: Htyp.t, t2: Htyp.t): bool => {
  switch (t1, t2) {
  | (Hole, _) => true
  | (_, Hole) => true
  | (Num, Num) => true
  | (Bool, Bool) => true
  | (Arrow(t11, t12), Arrow(t21, t22)) =>
    type_consistent(t11, t21) && type_consistent(t12, t22)
  | (Product(t11, t12), Product(t21, t22)) =>
    type_consistent(t11, t21) && type_consistent(t12, t22)
  | _ => false
  };
};

let rec syn = (ctx: typctx, e: Hexp.t): option(Htyp.t) => {
  // Marking is performed in the syn_action function
  switch (e) {
  | Var(name) => TypCtx.find_opt(name, ctx)
  | Lam(arg_name, type_asc, body) =>
    let* t2 = syn(TypCtx.add(arg_name, type_asc, ctx), body);
    Some(Htyp.Arrow(type_asc, t2));
  | Ap(func, arg) =>
    let* t1 = syn(ctx, func);
    let* (t2, t) = matched_arrow_typ(t1);
    if (ana(ctx, arg, t2)) {
      Some(t);
    } else {
      None;
    };
  | Let(var_name, bound, body) =>
    // let x = e1 in e2 -> t iff: e1 -> t1, ctx extend w x:t1, e2 -> t
    let* t1 = syn(ctx, bound);
    syn(TypCtx.add(var_name, t1, ctx), body);
  | Asc(e, t) =>
    if (ana(ctx, e, t)) {
      Some(t);
    } else {
      None;
    }
  | NumLit(_) => Some(Htyp.Num)
  | BoolLit(_) => Some(Htyp.Bool)
  | Prod(e1, e2) =>
    // (e1, e2) -> t iff: e1 -> t1, e2 -> t2
    let e1_syn = syn(ctx, e1);
    let e2_syn = syn(ctx, e2);
    // if both synthesize, return the product type
    // if either fails, have it return hole type
    switch (e1_syn, e2_syn) {
    | (Some(t1), Some(t2)) => Some(Htyp.Product(t1, t2))
    | (Some(t1), None) => Some(Htyp.Product(t1, Htyp.Hole))
    | (None, Some(t2)) => Some(Htyp.Product(Htyp.Hole, t2))
    | (None, None) => Some(Htyp.Product(Htyp.Hole, Htyp.Hole))
    };
  | Proj1(e) =>
    let* e_syn = syn(ctx, e);
    let* (t1, _) = matched_product_typ(e_syn);
    Some(t1);
  | Proj2(e) =>
    let* e_syn = syn(ctx, e);
    let* (_, t2) = matched_product_typ(e_syn);
    Some(t2);
  | Plus(arg1, arg2) =>
    // e1 + e2 -> t iff: e1 <- Num, e2 <- Num
    let e1_ana = ana(ctx, arg1, Htyp.Num);
    let e2_ana = ana(ctx, arg2, Htyp.Num);
    if (e1_ana && e2_ana) {
      Some(Htyp.Num);
    } else {
      None;
    };
  | Cond(p, then_branch, else_branch) =>
    // if p then t else e
    // 1. p <- Bool
    // 2. t -> t'
    // 3. e -> t''
    // t' ~ t'' => else marked hole
    let p_ana = ana(ctx, p, Htyp.Bool);
    let t_syn = syn(ctx, then_branch);
    let e_syn = syn(ctx, else_branch);
    // TODO: distinguish between then/else failing synthesis and not agreeing
    switch (p_ana, t_syn, e_syn) {
    | (true, Some(t1), Some(t2)) =>
      if (type_consistent(t1, t2)) {
        Some(t1);
      } else {
        None;
      }
    | _ => None
    };
  | EHole => Some(Htyp.Hole)
  | MarkHole(exp, _) =>
    switch (syn(ctx, exp)) {
    | Some(_) => Some(Htyp.Hole)
    | None => None
    }
  };
}
and ana = (ctx: typctx, e: Hexp.t, t: Htyp.t): bool => {
  switch (e) {
  | Lam(var_name, asc, body) =>
    // lambda analayzes against t if: t is arrow t1->t2, t1 ana against asc, ctx extend w x:asc, body analyzes against t2
    let a = matched_arrow_typ(t);
    switch (a) {
    | Some((t1, t2)) =>
      if (type_consistent(t1, asc)) {
        ana(TypCtx.add(var_name, asc, ctx), body, t2);
      } else {
        false;
      }
    | _ => false
    };
  | _ =>
    // Subsumption
    switch (syn(ctx, e)) {
    | Some(t') => type_consistent(t, t')
    | None => false
    }
  };
};

let rec move_typ = (t: Ztyp.t, a: Dir.t): Ztyp.t => {
  switch (t) {
  | Cursor(typ) =>
    switch (typ, a) {
    | (Arrow(t1, t2), Child(One)) => LArrow(Cursor(t1), t2)
    | (Arrow(t1, t2), Child(Two)) => RArrow(t1, Cursor(t2))
    | (Product(t1, t2), Child(One)) => LProd(Cursor(t1), t2)
    | (Product(t1, t2), Child(Two)) => RProd(t1, Cursor(t2))
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
  | LProd(zt1, t2) =>
    switch (a) {
    | Child(_) => LProd(move_typ(zt1, a), t2)
    | Parent =>
      switch (zt1) {
      | Cursor(t1) => Cursor(Product(t1, t2))
      | _ => LProd(move_typ(zt1, a), t2)
      }
    }
  | RProd(t1, zt2) =>
    switch (a) {
    | Child(_) => RProd(t1, move_typ(zt2, a))
    | Parent =>
      switch (zt2) {
      | Cursor(t2) => Cursor(Product(t1, t2))
      | _ => RProd(t1, move_typ(zt2, a))
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
    | (Let(name, e1, e2), Child(One)) => LLet(name, Cursor(e1), e2)
    | (Let(name, e1, e2), Child(Two)) => RLet(name, e1, Cursor(e2))
    | (Asc(e, t), Child(One)) => LAsc(Cursor(e), t)
    | (Asc(e, t), Child(Two)) => RAsc(e, Cursor(t))
    | (Prod(e1, e2), Child(One)) => LProd(Cursor(e1), e2)
    | (Prod(e1, e2), Child(Two)) => RProd(e1, Cursor(e2))
    | (Proj1(e), Child(One)) => Proj1(Cursor(e))
    | (Proj2(e), Child(One)) => Proj2(Cursor(e))
    | (Ap(e1, e2), Child(One)) => LAp(Cursor(e1), e2)
    | (Ap(e1, e2), Child(Two)) => RAp(e1, Cursor(e2))
    | (Plus(e1, e2), Child(One)) => LPlus(Cursor(e1), e2)
    | (Plus(e1, e2), Child(Two)) => RPlus(e1, Cursor(e2))
    | (Cond(p, t, e), Child(One)) => IfCond(Cursor(p), t, e)
    | (Cond(p, t, e), Child(Two)) => ThenCond(p, Cursor(t), e)
    | (Cond(p, t, e), Child(Three)) => ElseCond(p, t, Cursor(e))
    | (MarkHole(e, m), Child(One)) => MarkHole(Cursor(e), m)
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
  | LLet(name, e1, e2) =>
    switch (a, e1) {
    | (Parent, Cursor(e1)) => Cursor(Let(name, e1, e2))
    | (_, _) => LLet(name, move_action(e1, a), e2)
    }
  | RLet(name, e1, e2) =>
    switch (a, e2) {
    | (Parent, Cursor(e2)) => Cursor(Let(name, e1, e2))
    | (_, _) => RLet(name, e1, move_action(e2, a))
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
  | LProd(e1, e2) =>
    switch (a, e1) {
    | (Parent, Cursor(e1)) => Cursor(Prod(e1, e2))
    | (_, _) => LProd(move_action(e1, a), e2)
    }
  | RProd(e1, e2) =>
    switch (a, e2) {
    | (Parent, Cursor(e2)) => Cursor(Prod(e1, e2))
    | (_, _) => RProd(e1, move_action(e2, a))
    }
  | Proj1(e) => Proj1(move_action(e, a))
  | Proj2(e) => Proj2(move_action(e, a))
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
  | IfCond(zp, t, e) =>
    switch (a, zp) {
    | (Parent, Cursor(p)) => Cursor(Cond(p, t, e))
    | (_, _) => IfCond(move_action(zp, a), t, e)
    }
  | ThenCond(p, zt, e) =>
    switch (a, zt) {
    | (Parent, Cursor(t)) => Cursor(Cond(p, t, e))
    | (_, _) => ThenCond(p, move_action(zt, a), e)
    }
  | ElseCond(p, t, ze) =>
    switch (a, ze) {
    | (Parent, Cursor(e)) => Cursor(Cond(p, t, e))
    | (_, _) => ElseCond(p, t, move_action(ze, a))
    }
  | MarkHole(ze, m) =>
    switch (a, ze) {
    | (Parent, Cursor(t)) => Cursor(MarkHole(t, m))
    | (_, _) => MarkHole(move_action(ze, a), m)
    }
  };
};

let rec typ_action = (t: Ztyp.t, a: Action.t): option(Ztyp.t) => {
  switch (a) {
  | Move(dir) => Some(move_typ(t, dir))
  | _ =>
    switch (t) {
    | Cursor(typ) =>
      switch (a) {
      | Construct(shape) =>
        switch (typ, shape) {
        | (Hole, Num) => Some(Cursor(Num))
        | (Hole, Bool) => Some(Cursor(Bool))
        | (sub_t, Arrow) => Some(Cursor(Arrow(sub_t, Hole)))
        | _ => None
        }
      | Finish => None
      | Del => Some(Cursor(Hole))
      | Move(_) => None // this cant happen
      }
    | LArrow(zt, t) =>
      let* zt' = typ_action(zt, a);
      Some(Ztyp.LArrow(zt', t));
    | RArrow(t, zt) =>
      let* zt' = typ_action(zt, a);
      Some(Ztyp.RArrow(t, zt'));
    | LProd(zt, t) =>
      let* zt' = typ_action(zt, a);
      Some(Ztyp.LProd(zt', t));
    | RProd(t, zt) =>
      let* zt' = typ_action(zt, a);
      Some(Ztyp.RProd(t, zt'));
    }
  };
}
and syn_action =
    (ctx: typctx, (e: Zexp.t, t: Htyp.t), a: Action.t)
    : option((Zexp.t, Htyp.t)) => {
  switch (a) {
  | Move(dir) =>
    let e' = move_action(e, dir);
    Some((e', t));
  | _ =>
    switch (e) {
    | Cursor(h_exp) =>
      switch (a) {
      | Move(_) => None // this cant happen
      | Construct(shape) =>
        switch (shape) {
        | Arrow
        | Num
        | Bool
        | ProdT => None
        | Var(name) =>
          // Marking: MSFree
          let exp_opt =
            switch (TypCtx.find_opt(name, ctx)) {
            | Some(t') => Some((Zexp.Cursor(Var(name)), t'))
            | None => Some((MarkHole(Cursor(Var(name)), Free), Hole)) // free var, syn hole and put inside NE Hole
            };
          switch (h_exp) {
          | EHole => exp_opt
          | _ => None // cant construct a var on a non-hole
          };
        | Lam(name) =>
          switch (h_exp) {
          | EHole =>
            Some((Zexp.LLam(name, Cursor(Hole), EHole), Arrow(Hole, Hole)))
          | _ => None
          }
        | Asc =>
          let* t' = syn(ctx, h_exp);
          Some((Zexp.RAsc(h_exp, Cursor(t')), t'));
        | Ap =>
          let* t' = syn(ctx, h_exp);
          switch (matched_arrow_typ(t')) {
          | Some((_, t2)) => Some((Zexp.RAp(h_exp, Cursor(EHole)), t2))
          | None =>
            Some((
              Zexp.RAp(MarkHole(h_exp, UnexpectedType(t')), Cursor(EHole)),
              Hole,
            )) // arrow type expected
          };
        | NumLit(n) =>
          switch (h_exp) {
          | EHole => Some((Cursor(NumLit(n)), Htyp.Num))
          | _ => None
          }
        | BoolLit(b) =>
          switch (h_exp) {
          | EHole => Some((Cursor(BoolLit(b)), Htyp.Bool))
          | _ => None
          }
        | Cond =>
          let* t = syn(ctx, h_exp);
          if (type_consistent(t, Bool)) {
            Some((Zexp.IfCond(Cursor(h_exp), EHole, EHole), Htyp.Hole));
          } else {
            Some((Zexp.ThenCond(EHole, Cursor(h_exp), EHole), t));
          };
        | Plus =>
          let* t' = syn(ctx, h_exp);
          if (type_consistent(t', Htyp.Num)) {
            Some((Zexp.RPlus(h_exp, Cursor(EHole)), Htyp.Num));
          } else {
            Some((
              Zexp.RPlus(MarkHole(h_exp, InconsistentTypes), Cursor(EHole)),
              Htyp.Hole,
            ));
          };
        | Let(name) =>
          let* t' = syn(ctx, h_exp);
          Some((Zexp.LLet(name, Cursor(EHole), h_exp), t'));
        | Prod =>
          let* t' = syn(ctx, h_exp);
          Some((Zexp.RProd(h_exp, Cursor(EHole)), t'));
        | Proj1 =>
          let* t' = syn(ctx, h_exp);
          let* (t1, _) = matched_product_typ(t');
          Some((Zexp.Proj1(Cursor(h_exp)), t1));
        | Proj2 =>
          let* t' = syn(ctx, h_exp);
          let* (_, t2) = matched_product_typ(t');
          Some((Zexp.Proj2(Cursor(h_exp)), t2));
        | NEHole => raise(Unimplemented)
        }
      | Finish =>
        switch (h_exp) {
        | MarkHole(h_exp', m) =>
          switch (syn(ctx, h_exp')) {
          | Some(t) => Some((Zexp.Cursor(h_exp'), t))
          | None => Some((Zexp.MarkHole(Cursor(h_exp'), m), Hole))
          }
        | _ => None
        }
      | Del => Some((Cursor(EHole), Hole))
      }
    // zipper cases
    | LAp(z_exp, h_exp) =>
      let* t2 = syn(ctx, erase_exp(z_exp));
      let* (e', t3) = syn_action(ctx, (z_exp, t2), a);
      let* (t4, t5) = matched_arrow_typ(t3);
      let ana_correct = ana(ctx, h_exp, t4);
      if (ana_correct) {
        Some((Zexp.LAp(e', h_exp), t5));
      } else {
        None;
      };
    | RAp(h_exp, z_exp) =>
      let* t2 = syn(ctx, h_exp);
      let* (t3, t4) = matched_arrow_typ(t2);
      let* e' = ana_action(ctx, z_exp, a, t3);
      Some((Zexp.RAp(h_exp, e'), t4));
    | LLam(_, _, _) => None
    | RLam(_, _, _) => None
    | LPlus(z_exp, h_exp) =>
      let* z_exp' = ana_action(ctx, z_exp, a, Htyp.Num);
      Some((Zexp.LPlus(z_exp', h_exp), Htyp.Num));
    | RPlus(h_exp, z_exp) =>
      let* z_exp' = ana_action(ctx, z_exp, a, Htyp.Num);
      Some((Zexp.RPlus(h_exp, z_exp'), Htyp.Num));
    | IfCond(if_exp, then_exp, else_exp) =>
      let* z_exp' = ana_action(ctx, if_exp, a, Htyp.Bool);
      let* t = syn(ctx, then_exp);
      let* t' = syn(ctx, else_exp);
      switch (type_meet(t, t')) {
      | Some(t'') => Some((Zexp.IfCond(z_exp', then_exp, else_exp), t''))
      | None =>
        Some((
          Zexp.MarkHole(
            Zexp.IfCond(z_exp', then_exp, else_exp),
            InconsistentBranches,
          ),
          Hole,
        ))
      };
    | ThenCond(if_exp, then_exp, else_exp) =>
      let* t = syn(ctx, erase_exp(then_exp));
      let* z' = syn_action(ctx, (then_exp, t), a);
      let (then_exp', t) = z';
      let* t' = syn(ctx, else_exp);
      switch (type_meet(t, t')) {
      | Some(t'') => Some((Zexp.ThenCond(if_exp, then_exp', else_exp), t''))
      | None =>
        Some((
          Zexp.MarkHole(
            Zexp.ThenCond(if_exp, then_exp', else_exp),
            InconsistentBranches,
          ),
          Hole,
        ))
      };
    | ElseCond(if_exp, then_exp, else_exp) =>
      let* t = syn(ctx, erase_exp(else_exp));
      let* z' = syn_action(ctx, (else_exp, t), a);
      let (else_exp', t) = z';
      let* t' = syn(ctx, then_exp);
      switch (type_meet(t, t')) {
      | Some(t'') => Some((Zexp.ElseCond(if_exp, then_exp, else_exp'), t''))
      | None =>
        Some((
          Zexp.MarkHole(
            Zexp.ElseCond(if_exp, then_exp, else_exp'),
            InconsistentBranches,
          ),
          Hole,
        ))
      };
    | LLet(name, z_exp, h_exp) =>
      let* t = syn(ctx, erase_exp(z_exp));
      let* (e', t') = syn_action(TypCtx.add(name, t, ctx), (z_exp, t), a);
      let* t'' = syn(TypCtx.add(name, t', ctx), h_exp);
      Some((Zexp.LLet(name, e', h_exp), t''));
    | RLet(name, h_exp, z_exp) =>
      let* t = syn(ctx, h_exp);
      let* (e', t') = syn_action(TypCtx.add(name, t, ctx), (z_exp, t), a);
      Some((Zexp.RLet(name, h_exp, e'), t'));
    | LAsc(z_exp, h_typ) =>
      let* e' = ana_action(ctx, z_exp, a, h_typ);
      Some((Zexp.LAsc(e', h_typ), h_typ));
    | RAsc(h_exp, z_typ) =>
      let* t' = typ_action(z_typ, a);
      if (ana(ctx, h_exp, erase_typ(t'))) {
        Some((Zexp.RAsc(h_exp, z_typ), erase_typ(t')));
      } else {
        Some((
          Zexp.MarkHole(
            Zexp.RAsc(h_exp, z_typ),
            UnexpectedType(erase_typ(t')),
          ),
          Hole,
        ));
      };
    | LProd(z_exp, h_exp) =>
      let* t = syn(ctx, erase_exp(z_exp));
      let* t2 = syn(ctx, h_exp);
      Some((Zexp.LProd(z_exp, h_exp), Htyp.Product(t, t2)));
    | RProd(h_exp, z_exp) =>
      let* t = syn(ctx, h_exp);
      let* t2 = syn(ctx, erase_exp(z_exp));
      Some((Zexp.RProd(h_exp, z_exp), Htyp.Product(t, t2)));
    | Proj1(z_exp) =>
      let* t = syn(ctx, erase_exp(z_exp));
      let* (t1, _) = matched_product_typ(t);
      Some((Zexp.Proj1(z_exp), t1));
    | Proj2(z_exp) =>
      let* t = syn(ctx, erase_exp(z_exp));
      let* (_, t2) = matched_product_typ(t);
      Some((Zexp.Proj2(z_exp), t2));
    | MarkHole(z_exp, m) =>
      let t' =
        switch (syn(ctx, erase_exp(z_exp))) {
        | Some(t) => t
        | None => Hole
        };
      // perform syn action
      let* z' = syn_action(ctx, (z_exp, t'), a);
      let (z_exp', t) = z';
      Some((Zexp.MarkHole(z_exp', m), t));
    }
  };
}
and ana_action =
    (ctx: typctx, e: Zexp.t, a: Action.t, t: Htyp.t): option(Zexp.t) => {
  let subsumption =
      (ctx: typctx, e: Zexp.t, a: Action.t, t: Htyp.t): option(Zexp.t) => {
    let* t' = syn(ctx, erase_exp(e));
    let* (e', t'') = syn_action(ctx, (e, t'), a);
    if (type_consistent(t, t'')) {
      Some(e');
    } else {
      None;
    };
  };
  switch (a) {
  | Move(_) =>
    let* (zexp, t') = syn_action(ctx, (e, t), a);
    if (!type_consistent(t, t')) {
      raise(Unimplemented);
    };
    Some(zexp);
  | _ =>
    switch (e) {
    | Cursor(h_exp) =>
      switch (a) {
      | Construct(shape) =>
        switch (shape) {
        | Arrow
        | Num
        | Bool => None
        | Var(name) =>
          let* t' = TypCtx.find_opt(name, ctx);
          if (!type_consistent(t, t')) {
            Some(Zexp.MarkHole(Cursor(Var(name)), InconsistentTypes));
          } else {
            subsumption(ctx, e, a, t);
          };
        | Lam(name) =>
          switch (h_exp) {
          | EHole =>
            let a = matched_arrow_typ(t);
            switch (a) {
            | Some((_, _)) => Some(Zexp.RLam(name, Hole, Cursor(EHole)))
            | None =>
              Some(
                Zexp.MarkHole(
                  Zexp.RLam(name, Hole, Cursor(EHole)),
                  UnexpectedType(t),
                ),
              )
            };
          | _ => None
          }
        | NumLit(n) =>
          if (!type_consistent(t, Htyp.Num)) {
            Some(
              Zexp.MarkHole(Cursor(NumLit(n)), UnexpectedType(Htyp.Num)),
            );
          } else {
            subsumption(ctx, e, a, t);
          }
        | BoolLit(b) =>
          if (!type_consistent(t, Htyp.Bool)) {
            Some(MarkHole(Cursor(BoolLit(b)), UnexpectedType(Htyp.Bool)));
          } else {
            subsumption(ctx, e, a, t);
          }
        | Asc => Some(Zexp.RAsc(h_exp, Cursor(t)))
        | _ => subsumption(ctx, e, a, t)
        }
      | Finish =>
        switch (h_exp) {
        | MarkHole(h_exp', _) =>
          if (ana(ctx, h_exp', t)) {
            Some(Cursor(h_exp'));
          } else {
            None;
          }
        | _ => None
        }
      | Del => Some(Cursor(EHole))
      | Move(_) => None
      }
    // zipper
    | LLam(name, z_typ, h_exp) =>
      switch (matched_arrow_typ(t)) {
      | Some((t1, t2)) =>
        let* asc' = typ_action(z_typ, a);
        if (type_consistent(erase_typ(asc'), t1)) {
          if (ana(TypCtx.add(name, erase_typ(asc'), ctx), h_exp, t2)) {
            Some(Zexp.LLam(name, asc', h_exp));
          } else {
            Some(
              Zexp.MarkHole(
                Zexp.LLam(name, asc', h_exp),
                UnexpectedType(t2),
              ),
            );
          };
        } else {
          Some(
            Zexp.MarkHole(Zexp.LLam(name, asc', h_exp), UnexpectedType(t1)),
          );
        };
      | _ => None
      }
    | RLam(name, asc, h_exp) =>
      let* h_exp' = ana_action(ctx, h_exp, a, t);
      Some(Zexp.RLam(name, asc, h_exp'));
    | _ => subsumption(ctx, e, a, t)
    }
  };
};
