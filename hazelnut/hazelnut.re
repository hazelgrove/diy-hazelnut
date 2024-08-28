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
    | Bool
    | Hole;
};

module Hexp = {
  [@deriving (sexp, compare)]
  type t =
    | Var(string)
    | Lam(string, t)
    | Let(string, t, t)
    | Ap(t, t)
    | Lit(int)
    | BoolLit(bool)
    | Plus(t, t)
    | Cond(t, t, t)
    | Asc(t, Htyp.t)
    | EHole
    | NEHole(t);
};

module Ztyp = {
  [@deriving (sexp, compare)]
  type t =
    | Cursor(Htyp.t)
    | LArrow(t, Htyp.t)
    | RArrow(Htyp.t, t);
};

module Zexp = {
  [@deriving (sexp, compare)]
  type t =
    | Cursor(Hexp.t)
    | Lam(string, t)
    | LLet(string, t, Hexp.t)
    | RLet(string, Hexp.t, t)
    | LAp(t, Hexp.t)
    | RAp(Hexp.t, t)
    | LPlus(t, Hexp.t)
    | RPlus(Hexp.t, t)
    | PCond(t, Hexp.t, Hexp.t)
    | ThenCond(Hexp.t, t, Hexp.t)
    | ElseCond(Hexp.t, Hexp.t, t)
    | LAsc(t, Htyp.t)
    | RAsc(Hexp.t, Ztyp.t)
    | NEHole(t);
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
    | Asc
    | Var(string)
    | Lam(string)
    | Let(string)
    | Ap
    | Lit(int)
    | BoolLit(bool)
    | Cond
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
  // Used to suppress unused variable warnings
  // Okay to remove
  switch (e) {
  | Cursor(e') => e'
  | Lam(name, body) => Lam(name, erase_exp(body))
  | LAp(zt, et) => Ap(erase_exp(zt), et)
  | RAp(et, zt) => Ap(et, erase_exp(zt))
  | LPlus(zt, et) => Plus(erase_exp(zt), et)
  | RPlus(et, zt) => Plus(et, erase_exp(zt))
  | PCond(zp, ht, he) => Cond(erase_exp(zp), ht, he)
  | ThenCond(p, zt, he) => Cond(p, erase_exp(zt), he)
  | ElseCond(p, ht, ze) => Cond(p, ht, erase_exp(ze))
  | LAsc(zt, typ) => Asc(erase_exp(zt), typ)
  | RAsc(e, zt) => Asc(e, erase_typ(zt))
  | NEHole(zt) => NEHole(erase_exp(zt))
  };
}
and erase_typ = (t: Ztyp.t): Htyp.t => {
  // Used to suppress unused variable warnings
  // Okay to remove
  switch (t) {
  | Cursor(t) => t
  | LArrow(zt1, t2) => Arrow(erase_typ(zt1), t2)
  | RArrow(t1, zt2) => Arrow(t1, erase_typ(zt2))
  };
};

let matched_arrow_typ = (t: Htyp.t): option((Htyp.t, Htyp.t)) => {
  switch (t) {
  | Arrow(t1, t2) => Some((t1, t2))
  | Hole => Some((Hole, Hole))
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
  | _ => false
  };
};

let rec syn = (ctx: typctx, e: Hexp.t): option(Htyp.t) => {
  // Used to suppress unused variable warnings
  // Okay to remove
  switch (e) {
  | Var(name) => TypCtx.find_opt(name, ctx)
  | Lam(_arg_name, _body) => None
  | Ap(func, arg) =>
    // e1(e2) -> t iff: e1 : t1, t1 >> t2->t, e2 : t2
    // e1 : t1 in context
    let* t1 = syn(ctx, func);
    // t1 is consistent with an arrow type of t2 -> t
    let* (t2, t) = matched_arrow_typ(t1);
    // if arg analyzes against t2, all good
    if (ana(ctx, arg, t2)) {
      Some(t);
    } else {
      None;
    };
  | Let(var_name, bound, body) =>
    // let x = e1 in e2 -> t iff: e1 -> t1, ctx extend w x:t1, e2 -> t
    let* t1 = syn(ctx, bound);
    syn(TypCtx.add(var_name, t1, ctx), body);
  | Lit(_) => Some(Htyp.Num)
  | BoolLit(_) => Some(Htyp.Bool)
  | Plus(arg1, arg2) =>
    // e1 + e2 -> t iff: e1 <- Num, e2 <- Num
    let e1_ana = ana(ctx, arg1, Htyp.Num);
    let e2_ana = ana(ctx, arg2, Htyp.Num);
    if (e1_ana && e2_ana) {
      Some(Htyp.Num);
    } else {
      None;
    };
  | Asc(exp, typ) =>
    // e : t -> t iff: e <- t
    if (ana(ctx, exp, typ)) {
      Some(typ);
    } else {
      None;
    }
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
  | NEHole(exp) =>
    switch (syn(ctx, exp)) {
    | Some(_) => Some(Htyp.Hole)
    | None => None
    }
  };
}
and ana = (ctx: typctx, e: Hexp.t, t: Htyp.t): bool => {
  // Used to suppress unused variable warnings
  // Okay to remove
  switch (e) {
  | Lam(var_name, body) =>
    // lambda analayzes against t if: t is arrow t1->t2, ctx extend w x:t1, body analyzes against t2
    let a = matched_arrow_typ(t);
    switch (a) {
    | Some((t1, t2)) => ana(TypCtx.add(var_name, t1, ctx), body, t2)
    | _ => false
    };
  | _ =>
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
    | (Lam(name, body), Child(One)) => Lam(name, Cursor(body))
    | Let(name, e1, e2)
    | (Ap(e1, e2), Child(One)) => LAp(Cursor(e1), e2)
    | (Ap(e1, e2), Child(Two)) => RAp(e1, Cursor(e2))
    | (Plus(e1, e2), Child(One)) => LPlus(Cursor(e1), e2)
    | (Plus(e1, e2), Child(Two)) => RPlus(e1, Cursor(e2))
    | (Asc(e1, t1), Child(One)) => LAsc(Cursor(e1), t1)
    | (Asc(e1, t1), Child(Two)) => RAsc(e1, Cursor(t1))
    | (NEHole(e1), Child(One)) => NEHole(Cursor(e1))

    | _ => raise(Unimplemented)
    }
  | Lam(name, body) =>
    switch (a, body) {
    | (Parent, Cursor(body)) => Cursor(Lam(name, body))
    | (_, _) => Lam(name, move_action(body, a))
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
  | LAsc(zt, typ) =>
    switch (a, zt) {
    | (Parent, Cursor(t)) => Cursor(Asc(t, typ))
    | (_, _) => LAsc(move_action(zt, a), typ)
    }
  | RAsc(et, zt) =>
    switch (a, zt) {
    | (Parent, Cursor(t)) => Cursor(Asc(et, t))
    | (_, _) => RAsc(et, move_typ(zt, a))
    }
  | PCond(zp, t, e) =>
    switch (a, zt) {
    | (Parent, Cursor(p)) => Cursor(Cond(p, t, e))
    | (_, _) => PCond(move_action(zp, a), t, e)
    }
  | ThenCond(p, zt, e) =>
    switch (a, zt) {
    | (Parent, Cursor(t)) => Cursor(Cond(p, t, e))
    | (_, _) => ThenCond(p, move_action(zt, a), e)
    }
  | ThenCond(p, t, ze) =>
    switch (a, ze) {
    | (Parent, Cursor(e)) => Cursor(Cond(p, t, e))
    | (_, _) => ThenCond(p, t, move_action(ze, a))
    }
  | NEHole(zt) =>
    switch (a, zt) {
    | (Parent, Cursor(t)) => Cursor(NEHole(t))
    | (_, _) => NEHole(move_action(zt, a))
    }
  };
};

let rec typ_action = (t: Ztyp.t, a: Action.t): option(Ztyp.t) => {
  print_endline(
    "typ_action"
    ++ Sexplib.Sexp.to_string_hum(Action.sexp_of_t(a))
    ++ " "
    ++ Sexplib.Sexp.to_string_hum(Ztyp.sexp_of_t(t)),
  );
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
      print_endline(
        "zipper L" ++ Sexplib.Sexp.to_string_hum(Ztyp.sexp_of_t(zt')),
      );
      Some(Ztyp.LArrow(zt', t));
    | RArrow(t, zt) =>
      let* zt' = typ_action(zt, a);
      print_endline(
        "zipper R" ++ Sexplib.Sexp.to_string_hum(Ztyp.sexp_of_t(zt')),
      );
      Some(Ztyp.RArrow(t, zt'));
    }
  };
}
and syn_action =
    (ctx: typctx, (e: Zexp.t, t: Htyp.t), a: Action.t)
    : option((Zexp.t, Htyp.t)) => {
  print_endline(
    "syn_action"
    ++ Sexplib.Sexp.to_string_hum(Action.sexp_of_t(a))
    ++ " "
    ++ Sexplib.Sexp.to_string_hum(Zexp.sexp_of_t(e))
    ++ " "
    ++ Sexplib.Sexp.to_string_hum(Htyp.sexp_of_t(t)),
  );
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
        | Bool => None
        | Asc =>
          let t' =
            switch (syn(ctx, h_exp)) {
            | Some(t') => Some((Zexp.RAsc(h_exp, Cursor(t')), t))
            | None => Some((Zexp.RAsc(NEHole(h_exp), Cursor(Hole)), t)) // marking: MSAsc
            };
          (); // marking: MSAsc
        | Var(name) =>
          // Marking: MSFree
          let exp_opt =
            switch (TypCtx.find_opt(name, ctx)) {
            | Some(t') => Some((Zexp.Cursor(Var(name)), t'))
            | None => Some((NEHole(Cursor(Var(name))), Hole)) // free var, syn hole and put inside NE Hole
            };
          switch (h_exp) {
          | EHole => exp_opt
          | _ => None // cant construct a var on a non-hole
          };
        | Lam(name) =>
          switch (h_exp) {
          | EHole =>
            Some((
              Zexp.RAsc(Lam(name, EHole), LArrow(Cursor(Hole), Hole)),
              Arrow(Hole, Hole),
            ))
          | _ => None
          }
        | Ap =>
          let* t' = syn(ctx, h_exp);
          switch (matched_arrow_typ(t')) {
          | Some((_, t2)) => Some((Zexp.RAp(h_exp, Cursor(EHole)), t2))
          | None => Some((Zexp.RAp(NEHole(h_exp), Cursor(EHole)), Hole))
          };
        | Lit(n) =>
          switch (h_exp) {
          | EHole => Some((Cursor(Lit(n)), Htyp.Num))
          | _ => None
          }
        | BoolLit(b) =>
          switch (h_exp) {
          | EHole => Some((Cursor(BoolLit(b)), Htyp.Bool))
          | _ => None
          }
        | Cond =>
          // my cond has no else, so no marking
          // needed aside from the bool in the condition
          let* t' = syn(ctx, h_exp);
          if (type_consistent(t', Htyp.Bool)) {
            Some((Zexp.RCond(h_exp, Cursor(EHole)), Htyp.Hole));
          } else {
            Some((Zexp.RCond(NEHole(h_exp), Cursor(EHole)), Htyp.Hole));
          };
        | Plus =>
          let* t' = syn(ctx, h_exp);
          if (type_consistent(t', Htyp.Num)) {
            Some((Zexp.RPlus(h_exp, Cursor(EHole)), Htyp.Num));
          } else {
            Some((Zexp.RPlus(NEHole(h_exp), Cursor(EHole)), Htyp.Hole));
          };
        | NEHole => Some((NEHole(Cursor(h_exp)), Htyp.Hole))
        }
      | Finish =>
        switch (h_exp) {
        | NEHole(h_exp') =>
          let* t = syn(ctx, h_exp);
          Some((Zexp.Cursor(h_exp'), t));
        | _ => None
        }
      | Del => Some((Cursor(EHole), Hole))
      }
    // zipper
    | LAsc(z_exp, h_typ) =>
      // if e' analyzes against t, return e', t
      let* z_exp = ana_action(ctx, z_exp, a, h_typ);
      Some((Zexp.LAsc(z_exp, h_typ), h_typ));
    | RAsc(h_exp, z_typ) =>
      let* t' = typ_action(z_typ, a);
      print_endline(
        "RAsc" ++ Sexplib.Sexp.to_string_hum(Ztyp.sexp_of_t(t')),
      );
      if (ana(ctx, h_exp, erase_typ(t'))) {
        print_endline("RAsc success");
        Some((Zexp.RAsc(h_exp, t'), erase_typ(t')));
      } else {
        print_endline("RAsc failed");
        None;
      };
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
    | Lam(_, _) => None
    | LPlus(z_exp, h_exp) =>
      let* z_exp' = ana_action(ctx, z_exp, a, Htyp.Num);
      Some((Zexp.LPlus(z_exp', h_exp), Htyp.Num));
    | RPlus(h_exp, z_exp) =>
      let* z_exp' = ana_action(ctx, z_exp, a, Htyp.Num);
      Some((Zexp.RPlus(h_exp, z_exp'), Htyp.Num));
    | LCond(z_exp, h_exp) =>
      let* z_exp' = ana_action(ctx, z_exp, a, Htyp.Bool);
      let* t = syn(ctx, h_exp);
      Some((Zexp.LCond(z_exp', h_exp), t));
    | RCond(h_exp, z_exp) =>
      let* t' = syn(ctx, erase_exp(z_exp));
      let* (e', t'') = syn_action(ctx, (z_exp, t'), a);
      Some((Zexp.RCond(h_exp, e'), t''));
    | NEHole(z_exp) =>
      let* t' = syn(ctx, erase_exp(z_exp));
      let* (e', t'') = syn_action(ctx, (z_exp, t'), a);
      Some((Zexp.NEHole(e'), t''));
    }
  };
}
and ana_action =
    (ctx: typctx, e: Zexp.t, a: Action.t, t: Htyp.t): option(Zexp.t) => {
  print_endline(
    "ana_action"
    ++ Sexplib.Sexp.to_string_hum(Action.sexp_of_t(a))
    ++ " "
    ++ Sexplib.Sexp.to_string_hum(Zexp.sexp_of_t(e))
    ++ " "
    ++ Sexplib.Sexp.to_string_hum(Htyp.sexp_of_t(t)),
  );
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
        | Asc => Some(RAsc(h_exp, Cursor(t)))
        | Var(name) =>
          let* t' = TypCtx.find_opt(name, ctx);
          if (!type_consistent(t, t')) {
            Some(Zexp.NEHole(Cursor(Var(name))));
          } else {
            subsumption(ctx, e, a, t);
          };
        | Lam(name) =>
          switch (h_exp) {
          | EHole =>
            let a = matched_arrow_typ(t);
            switch (a) {
            | Some((_, _)) => Some(Zexp.Lam(name, Cursor(EHole)))
            | None =>
              Some(
                Zexp.NEHole(
                  RAsc(Lam(name, EHole), LArrow(Cursor(Hole), Hole)),
                ),
              )
            };
          | _ => None
          }
        | Lit(n) =>
          if (!type_consistent(t, Htyp.Num)) {
            Some(NEHole(Cursor(Lit(n))));
          } else {
            subsumption(ctx, e, a, t);
          }
        | BoolLit(b) =>
          if (!type_consistent(t, Htyp.Bool)) {
            Some(NEHole(Cursor(BoolLit(b))));
          } else {
            subsumption(ctx, e, a, t);
          }
        | _ => subsumption(ctx, e, a, t)
        }
      | Finish =>
        switch (h_exp) {
        | NEHole(h_exp') =>
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
    | Lam(name, z_exp) =>
      let* (t1, t2) = matched_arrow_typ(t);
      let* z_exp' = ana_action(TypCtx.add(name, t1, ctx), z_exp, a, t2);
      Some(Zexp.Lam(name, z_exp'));
    | _ => subsumption(ctx, e, a, t)
    }
  };
};
