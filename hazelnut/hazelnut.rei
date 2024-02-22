module Htyp: {
  [@deriving (sexp, compare)]
  type t =
    | Arrow(t, t)
    | Num
    | Hole;
};

module Hexp: {
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

module Ztyp: {
  [@deriving (sexp, compare)]
  type t =
    | Cursor(Htyp.t)
    | LArrow(t, Htyp.t)
    | RArrow(Htyp.t, t);
};

module Zexp: {
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

module Child: {
  type t =
    | One
    | Two;
};

module Dir: {
  type t =
    | Child(Child.t)
    | Parent;
};

module Shape: {
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

module Action: {
  [@deriving (sexp, compare)]
  type t =
    | Move(Dir.t)
    | Construct(Shape.t)
    | Del
    | Finish;
};

module TypCtx: {
  type t('a) = Map.Make(String).t('a);
  let empty: t('a);
};
type typctx = TypCtx.t(Htyp.t);

exception Unimplemented;

let erase_exp: Zexp.t => Hexp.t;
let syn: (typctx, Hexp.t) => option(Htyp.t);
let ana: (typctx, Hexp.t, Htyp.t) => bool;
let syn_action:
  (typctx, (Zexp.t, Htyp.t), Action.t) => option((Zexp.t, Htyp.t));
let ana_action: (typctx, Zexp.t, Action.t, Htyp.t) => option(Zexp.t);
