module Htyp: {
  [@deriving (sexp, compare)]
  type t =
    | Arrow(t, t)
    | Num
    | Hole;
};

module Ztyp: {
  [@deriving (sexp, compare)]
  type t =
    | Cursor(Htyp.t)
    | LArrow(t, Htyp.t)
    | RArrow(Htyp.t, t);
};

module Mark: {
  [@deriving (sexp, compare)]
  type t =
    | Free
    | NonArrowAp
    | LamNonArrow
    | Inconsistent;
};

module Hexp: {
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

module Zexp: {
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

module Child: {
  [@deriving (sexp, compare)]
  type t =
    | One
    | Two
    | Three;
};

module Dir: {
  [@deriving (sexp, compare)]
  type t =
    | Child(Child.t)
    | Parent;
};

module Shape: {
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

module Action: {
  [@deriving (sexp, compare)]
  type t =
    | Move(Dir.t)
    | Construct(Shape.t)
    | Del;
};

module TypCtx: {
  type t('a) = Map.Make(String).t('a);
  let empty: t('a);
};
type typctx = TypCtx.t(Htyp.t);

exception Unimplemented;

let erase_exp: Zexp.t => Hexp.t;
// let syn: (typctx, Hexp.t) => option(Htyp.t);
// let ana: (typctx, Hexp.t, Htyp.t) => bool;
let exp_action: (Zexp.t, Action.t) => Zexp.t;
let mark_syn: (typctx, Hexp.t) => (Hexp.t, Htyp.t);
let fold_zexp_mexp: (Zexp.t, Hexp.t) => Zexp.t;
