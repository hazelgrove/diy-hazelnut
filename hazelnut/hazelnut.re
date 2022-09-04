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

let erase_typ: ztyp => htyp = _ => raise(Unimplemented);

let erase_exp: zexp => hexp = _ => raise(Unimplemented);

let exp_movement = (e: zexp, d: dir): option(zexp) => {
  let _ = e;
  let _ = d;
  raise(Unimplemented);
};

let typ_action = (t: ztyp, a: action): option(ztyp) => {
  let _ = t;
  let _ = a;
  raise(Unimplemented);
};

let syn_action =
    (ctx: typctx, (e: zexp, t: htyp), a: action): option((zexp, htyp)) => {
  let _ = ctx;
  let _ = e;
  let _ = t;
  let _ = a;
  raise(Unimplemented);
}
and ana_action = (ctx: typctx, e: zexp, a: action, t: htyp): zexp => {
  let _ = ctx;
  let _ = e;
  let _ = a;
  let _ = t;
  raise(Unimplemented);
};

let _ = exp_movement;
let _ = typ_action;
let _ = erase_exp;
let _ = erase_typ;
