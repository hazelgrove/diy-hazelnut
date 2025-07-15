open Alcotest;
module Hazelnut = Hazelnut_lib.Hazelnut;

let hexp_eq = (he1: Hazelnut.Hexp.t, he2: Hazelnut.Hexp.t): bool =>
  Hazelnut.Hexp.compare(he1, he2) == 0;

let hexp_print = (e: Hazelnut.Hexp.t): string => Hazelnut.Hexp.show(e);

let hexp_typ = testable(Fmt.using(hexp_print, Fmt.string), hexp_eq);

let htyp_eq =
    (ht1: option(Hazelnut.Htyp.t), ht2: option(Hazelnut.Htyp.t)): bool =>
  switch (ht1, ht2) {
  | (Some(t1), Some(t2)) => Hazelnut.Htyp.compare(t1, t2) == 0
  | (None, None) => true
  | _ => false
  };

let htyp_print = (ht: option(Hazelnut.Htyp.t)): string =>
  switch (ht) {
  | Some(t) => Hazelnut.Htyp.show(t)
  | _ => "None"
  };

let htyp_typ = testable(Fmt.using(htyp_print, Fmt.string), htyp_eq);

let zexp_eq =
    (ze1: option(Hazelnut.Zexp.t), ze2: option(Hazelnut.Zexp.t)): bool =>
  switch (ze1, ze2) {
  | (Some(e1), Some(e2)) => Hazelnut.Zexp.compare(e1, e2) == 0
  | (None, None) => true
  | _ => false
  };

let zexp_print = (ze: option(Hazelnut.Zexp.t)): string =>
  switch (ze) {
  | Some(e) => Hazelnut.Zexp.show(e)
  | _ => "None"
  };

let zexp_typ = testable(Fmt.using(zexp_print, Fmt.string), zexp_eq);

let zexp_htyp_eq =
    (
      ze_t1: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)),
      ze_t2: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)),
    )
    : bool =>
  switch (ze_t1, ze_t2) {
  | (Some((e1, t1)), Some((e2, t2))) =>
    Hazelnut.Zexp.compare(e1, e2) == 0 && Hazelnut.Htyp.compare(t1, t2) == 0
  | _ => false
  };

let zexp_htyp_print =
    (ze_t: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t))): string =>
  switch (ze_t) {
  | Some((e, _)) => Hazelnut.Zexp.show(e) // have to show both somehow
  | _ => "None"
  };

let zexp_htyp =
  testable(Fmt.using(zexp_htyp_print, Fmt.string), zexp_htyp_eq);
