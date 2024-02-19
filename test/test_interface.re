open Alcotest;
module Hazelnut = Hazelnut_lib.Hazelnut;

let hexp_eq = (he1: Hazelnut.Hexp.t, he2: Hazelnut.Hexp.t): bool =>
  Hazelnut.Hexp.compare(he1, he2) == 0;

let hexp_print = (_: Hazelnut.Hexp.t): string => "hexp";

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
  | Some(_) => "htyp"
  | _ => "None"
  };

let htyp_typ = testable(Fmt.using(htyp_print, Fmt.string), htyp_eq);
