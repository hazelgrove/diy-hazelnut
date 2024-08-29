module Htyp: {
  [@deriving (sexp, compare)]
  type t =
    | Arrow(t, t)
    | Product(t, t)
    | Num
    | Bool
    | Hole;
};

module Mark: {
  [@deriving (sexp, compare)]
  type t =
    | InconsistentTypes
    | InconsistentBranches
    | InconsistentAscription
    | Free
    | UnexpectedType(Htyp.t);
};

module Hexp: {
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

module Ztyp: {
  [@deriving (sexp, compare)]
  type t =
    | Cursor(Htyp.t)
    | LArrow(t, Htyp.t)
    | RArrow(Htyp.t, t)
    | LProd(t, Htyp.t)
    | RProd(Htyp.t, t);
};

module Zexp: {
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

module Child: {
  type t =
    | One
    | Two
    | Three;
};

module Dir: {
  type t =
    | Child(Child.t)
    | Parent;
};

module Shape: {
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
