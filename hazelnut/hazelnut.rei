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
