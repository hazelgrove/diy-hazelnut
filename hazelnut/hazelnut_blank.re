open Sexplib.Std;
// open Monad_lib.Monad; // Uncomment this line to use the maybe monad

let compare_string = String.compare;
let compare_int = Int.compare;
// let compare_bool = Bool.compare;

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
    | LamAscIncon
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

let erase_exp = (e: Zexp.t): Hexp.t => {
  // Used to suppress unused variable warnings
  // Okay to remove
  let _ = e;

  raise(Unimplemented);
};

let mark_syn = (ctx: typctx, e: Hexp.t): (Hexp.t, Htyp.t) => {
  // Used to suppress unused variable warnins
  // Okay to remove
  let _ = ctx;
  let _ = e;

  raise(Unimplemented);
}

and mark_ana = (ctx: typctx, t: Htyp.t, e: Hexp.t): Hexp.t => {
  // Used to suppress unused variable warnings
  // Okay to remove
  let _ = ctx;
  let _ = e;
  let _ = t;

  raise(Unimplemented);
};

let perform_action = (e: Zexp.t, a: Action.t): Zexp.t => {
  // Used to suppress unused variable warnings
  // Okay to remove
  let _ = e;
  let _ = a;

  raise(Unimplemented);
};
