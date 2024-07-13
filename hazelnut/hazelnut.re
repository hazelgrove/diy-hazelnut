open Sexplib.Std;
open Monad_lib.Monad; // Uncomment this line to use the maybe monad

let compare_string = String.compare;
let compare_int = Int.compare;

module Htyp = {
  [@deriving (sexp, compare)]
  type t =
    | Arrow(t, t)
    | Num
    | Hole;
};

module Hexp = {
  [@deriving (sexp, compare)]
  type t =
    | Var(string)
    | Lam(string, t)
    | Ap(t, t)
    | Lit(int)
    | Plus(t, t)
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
    | LAp(t, Hexp.t)
    | RAp(Hexp.t, t)
    | LPlus(t, Hexp.t)
    | RPlus(Hexp.t, t)
    | LAsc(t, Htyp.t)
    | RAsc(Hexp.t, Ztyp.t)
    | NEHole(t);
};

module Child = {
  [@deriving (sexp, compare)]
  type t =
    | One
    | Two;
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
    | Asc
    | Var(string)
    | Lam(string)
    | Ap
    | Lit(int)
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
exception Invalid;

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
  | LAsc(zt, typ) => Asc(erase_exp(zt), typ);
  | NEHole(zt) => NEHole(erase_exp(t));
  | RAsc(e, zt) => Asc(e, erase_typ(zt))
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

let type_consistent = (t1: Htyp.t, t2: Htyp.t) => bool {
    switch(t1, t2){
    | (Hole, _) => true
    | (_, Hole) => true
    | (Num, Num) => true
    | (Arrow(t11, t12), Arrow(t21, t22)) => type_consistent(t11, t21) && type_consistent(t12, t22)
    | _ => false
    }
}

let rec syn = (ctx: typctx, e: Hexp.t): option(Htyp.t) => {
  // Used to suppress unused variable warnings
  // Okay to remove
  switch (e) {
  | Var(name) => TypCtx.find_opt(name, ctx)
  | Lam(_arg_name, _body) => raise(Invalid)
  | Ap(func, arg) =>
        // e1(e2) -> t iff: e1 : t1, t1 >> t2->t, e2 : t2
        // e1 : t1 in context
        let* t1 = syn(ctx, func);
        // t1 is consistent with an arrow type of t2 -> t
        let* (t2, t) = matched_arrow_typ(t1);
        // if arg analyzes against t2, all good
        if(ana(ctx, arg, t2)) {
        Some(t);
        } else {
        None;
        }
  | Lit(_) => return Some(Htyp.Num)
  | Plus(arg1, arg2) =>
        // e1 + e2 -> t iff: e1 <- Num, e2 <- Num
        let e1_ana = ana(ctx, arg1, Htyp.Num);
        let e2_ana = ana(ctx, arg2, Htyp.Num);
        if(e1_ana && e2_ana) {
        Some(Htyp.Num);
        } else {
        None;
        }
  | Asc(exp, typ) =>
        // e : t -> t iff: e <- t
        if(ana(ctx, exp, typ)) {
        Some(typ);
        } else {
        None;
        }
  | EHole => return Some(Htyp.Hole)
  | NEHole(exp) =>
        switch(syn(ctx, exp)){
            | Some(typ) => Some(Htyp.Hole)
            | None => None
        }
  };
}
and ana = (ctx: typctx, e: Hexp.t, t: Htyp.t): bool => {
    // Used to suppress unused variable warnings
    // Okay to remove
    let _ = ctx;
    let _ = e;
    let _ = t;
    switch(e){
    | Lam(var_name, body) =>
        // lambda analayzes against t if: t is arrow t1->t2, ctx extend w x:t1, body analyzes against t2
        let* (t1, t2) = matched_arrow_typ(t);
        ana(TypCtx.add(var_name, t1, ctx), body, t2)
    | _ => syn(ctx, e) == Some(type_consistent(t));
    }
};

let rec move_typ = (t : Ztyp.t, a: Dir.t) : Ztyp.t => {
    switch(t){
    | Cursor(typ) =>
        switch(typ, a){
        | (Arrow(t1, t2), Child(One)) => LArrow(Cursor(t1), t2)
        | (Arrow(t1, t2), Child(Two)) => RArrow(t1, Cursor(t2))
        | _ => raise(Invalid)
        }
    | LArrow(zt1, t2) =>
        switch(a){
        | Child(_) => LArrow(move_typ(zt1, a), t2)
        | Parent =>
            switch(zt1){
            | Cursor(t1) => Cursor(Arrow(t1, t2))
            | _ => LArrow(move_typ(zt1, a), t2)
            }
        }
    | RArrow(t1, zt2) =>
        | Child(_) => RArrow(t1, move_typ(zt2, a))
        | Parent =>
            switch(zt2){
            | Cursor(t2) => Cursor(Arrow(t1, t2))
            | _ => RArrow(t1, move_typ(zt2, a))
            }
    }
};

let rec move_action = (e: Zexp.t, a: Dir.t) : Zexp.t => {
    switch (e) {
    | Cursor(e') => // child movements here
        switch(e', a){
        | (Lam(name, body), Child(One)) => Lam(name, Cursor(body))
        | (Ap(e1, e2), Child(One)) => LAp(Cursor(e1), e2)
        | (Ap(e1, e2), Child(Two)) => RAp(e1, Cursor(e2))
        | (Plus(e1, e2), Child(One)) => LPlus(Cursor(e1), e2)
        | (Plus(e1, e2), Child(Two)) => RPlus(e1, Cursor(e2))
        | (Asc(e1, t1), Child(One)) => LAsc(Cursor(e1), t2)
        | (Asc(e1, t1), Child(Two)) => RAsc(e1, Cursor(t1))
        | (NEHole(e1), Child(One)) => NEHole(Cursor(e1))
        | _ => raise(Invalid)
        }
    | Lam(name, body) =>
        switch(a, body){
        | Parent, Cursor(body) => Cursor(Lam(name, body))
        | _, _ => Lam(name, move_action(body, a))
        }
    | LAp(zt, et) =>
        switch(a, zt){
        | Parent, Cursor(t) => Cursor(Ap(t, et))
        | _, _ => LAp(move_action(zt, a), et)
        }
    | RAp(et, zt) =>
        switch(a, zt){
        | Parent, Cursor(t) => Cursor(Ap(et, t))
        | _, _ => RAp(et, move_action(zt, a))
        }
    | LPlus(zt, et) =>
        switch(a, zt){
        | Parent, Cursor(t) => Cursor(Plus(t, et))
        | _, _ => LPlus(move_action(zt, a), et)
        }
    | RPlus(et, zt) =>
        switch(a, zt){
        | Parent, Cursor(t) => Cursor(Plus(et, t))
        | _, _ => RPlus(et, move_action(zt, a))
        }
    | LAsc(zt, typ) =>
        switch(a, zt){
        | Parent, Cursor(t) => Cursor(Asc(t, typ))
        | _, _ => LAsc(move_action(zt, a), typ)
        }
    | RAsc(a, zt) =>
        switch(a, zt){
        | Parent, Cursor(t) => Cursor(Asc(a, t))
        | _, _ => RAsc(a, move_typ(zt, a))
        }
    | NEHole(zt) => NEHole(erase_exp(t)) =>
        switch(a, zt){
        | Parent, Cursor(t) => Cursor(NEHole(t))
        | _, _ => NEHole(move_action(zt, a))
        }
    };
};
let construct_type = (t: Ztyp.t, shape: Shape.t): option(Ztyp.t) =>{
    switch(t){
    | Cursor(typ) =>
        switch(typ, shape){
        | sub_t, Arrow => Some(Cursor(Arrow(sub_t, Hole)))
        | Hole, Num => Some(Cursor(Num))
        | _ => None
        }
    | LArrow(zt, t2) =>
        let* t = construct_type(zt, shape);
        Some(LArrow(t, t2))
    | RArrow(t1, zt) =>
        let* t = construct_type(zt, shape);
        Some(RArrow(t1, t))
    }
};

let shape_to_h_exp = (shape: Shape.t): Hexp.t => {
    switch(shape){
    | Var(name) => Var(name)
    | Lam(name) => Lam(name, EHole)
    | Ap => EHole
    | Lit(i) => Lit(i)
    | Plus => EHole
    | Asc => EHole
    | NEHole => NEHole(EHole)
    }
};

let construct_exp = (ctx: typctx, e: Zexp.t, shape: Shape.t): option(Zexp.t) => {
    switch(e){
    | Cursor(h_exp) =>
        switch(shape){
        | Arrow
        | Num => raise Invalid;
        | Asc =>
            // if in syn position, the hole is filled with the synthesized type of the h_exp, in ana it is filled with the type being analyzed against
            Some(RAsc(h_exp, Cursor(Hole)));
        | Var(string) =>

        | Lam(string)
        | Ap
        | Lit(int)
        | Plus
        | NEHole;
        }

    | _ => None // zipper cases not handled here, simply return None if given a zipper case
    }
};

let rec syn_action =
    (ctx: typctx, (e: Zexp.t, t: Htyp.t), a: Action.t)
    : option((Zexp.t, Htyp.t)) => {
    switch(a){
    | Move(dir) =>
        let e' = move_action(e,  dir);
        (e', t);
    | Construct(shape) =>
        switch(e){
        | Cursor(h_exp) =>
            switch(shape){
            | Arrow
            | Num => raise Invalid;
            | Asc =>
                let* t' = syn(ctx, h_exp);
                Some(RAsc(h_exp, Cursor(t')), t');
            | Var(name) =>
                let* t' = TypCtx.find_opt(name, ctx);
                switch(h_exp){
                | EHole => Some(Cursor(Var(name)), t');
                | _ => None; // invalid construction
                }
            | Lam(name) =>
                switch(h_exp){
                | EHole =>
                    let zt' = RAsc(Lam(name, EHole), LArrow(Cursor(Hole), Hole));
                    let t' = Arrow(Hole, Hole);
                    Some(zt', t');
                | _ => None; // invalid construction
                }

            | Ap =>
                let* t' = syn(ctx, h_exp);
                if(type_consistent(t', Arrow(Hole, Hole))){
                    Some(RAp(h_exp, Cursor(EHole)), Hole);
                } else {
                    Some(RAp(NEHole(h_exp), Cursor(EHole)), Hole);
                };
            | Lit(n) =>
                switch(h_exp){
                | EHole => Some(Cursor(Lit(n)), Htyp.Num);
                | _ => None; // invalid construction
                }
            | Plus =>
                let* t' = syn(ctx, h_exp);
                if(type_consistent(t', Htyp.Num)){
                    Some(RPlus(h_exp, Cursor(EHole)), Htyp.Num);
                } else {
                    Some((RPlus(NEHole(h_exp),Cursor(EHole))), Htyp.Num);
                };
            | NEHole => raise Invalid;
            }
        // zipper rules
        | _ => raise Unimplemented;
        }
    | Del =>
        switch(e){
        | Cursor(h_exp) =>
            Some(Cursor(EHole), Htyp.Hole);
        | _ => zipper_action(ctx, e, a, t);
        }
    | Finish =>
        switch(e){
        | Cursor(NEHole(hexp)) =>
            let* t' = syn(ctx, hexp);
            Some(Cursor(hexp), t');
        | Cursor(other) => None;
        | _ => syn_zipper_action(ctx, e, a, t);
        }
    }
}
and ana_action =
    (ctx: typctx, e: Zexp.t, a: Action.t, t: Htyp.t): option(Zexp.t) => {
    let subsumption = (ctx: typctx, e: Zexp.t, a: Action.t, t: Htyp.t): option(Zexp.t) => {
        let* t' = syn(ctx, erase_exp(e));
        let* (e', t'') = syn_action(ctx, (e, t), a);
        if(type_consistent(t, t'')){
            Some(e');
        } else {
            None;
        }
    };
    switch (a) {
    | Move(dir) =>
        let* (zexp, t') = syn_action(ctx, (e, t), a);
        if(!type_consistent(t, t')){
            raise Invalid; // this cant happen
        }
        Some(zexp);
    | Construct(shape) =>
        switch(e){
        | Cursor(h_exp) =>
            // perform construction
            switch(shape){
            | Arrow
            | Num => raise Invalid;
            | Asc =>
                RAsc(h_exp, Cursor(t));
            | Var(name) =>
                let* t' = TypCtx.find_opt(name, ctx);
                switch(h_exp){
                | EHole =>
                    if(!type_consistent(t', t)){
                        Some(NEHole(Cursor(Var(name))));
                    } else {
                        subsumption(ctx, e, a, t);
                    }
                | _ => None; // invalid construction
                }
            | Lam(name) =>
                let a = matched_arrow_typ(t);
                switch(e){
                | EHole =>
                    switch(a){
                    | Some((t1, t2)) =>
                        Some(Lam(name, Cursor(EHold)));
                    | None =>
                        Some(NEHole(RAsc(Lam(name, EHole), LArrow(Cursor(Hole), Hole))));
                    }
                | _ => None; // invalid construction
                }
            | Num(n) =>
                switch(e){
                | EHole =>
                    if(!type_consistent(t, Htyp.Num)){
                        Some(NEHole(Cursor(Lit(n))));
                    } else {
                        Some(Cursor(Lit(n)));
                    };
                | _ => None; // invalid construction
                }
            | Ap
            | Plus
            | NEHole => raise Invalid; // cant get here, should it subsume?
            }
        | _ => raise Unimplemented;
        }
    | Del =>
        switch(e){
        | Cursor(h_exp) =>
            Some(Cursor(EHole));
        | _ => zipper_action(ctx, e, a, t);
        }
    | Finish =>
        switch(e){
        | Cursor(NEHole(hexp)) =>
            // analyze the hole against the type
            let consistent = ana(ctx, hexp, t);
            if(consistent){
                Some(Cursor(hexp));
            } else {
                Some(NEHole(Cursor(hexp)));
            }
        | Cursor(other) => None;
        | _ => zipper_action(ctx, e, a, t);
        }
    }
} and syn_zipper_action = (ctx: typctx, (e: Zexp.t, t: Htyp.t), a: Action.t)
    : option((Zexp.t, Htyp.t)) => {
    switch(a){
    | Move(dir) => syn_action(ctx, (e, t), a);
    | _ =>
         switch(e){
        | Cursor(h) => syn_action(ctx, (h, t), a);
        | LAsc(z_exp, h_typ) =>
            let* z_exp' = ana_action(z_exp, a, h_typ);
            Some(LAsc(z_exp', h_typ), h_typ);
        | RAsc(h_exp, z_typ) =>
            // type action

        }
    }

    };
