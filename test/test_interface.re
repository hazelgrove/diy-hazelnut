open Alcotest;
module Hazelnut = Hazelnut_lib.Hazelnut;

let hexp_eq = (he1: Hazelnut.Hexp.t, he2: Hazelnut.Hexp.t): bool =>
  Hazelnut.Hexp.compare(he1, he2) == 0;

let hexp_print = (_: Hazelnut.Hexp.t): string => "hexp";

let hexp_typ = testable(Fmt.using(hexp_print, Fmt.string), hexp_eq);

let htyp_eq = (t1: Hazelnut.Htyp.t, t2: Hazelnut.Htyp.t): bool =>
  Hazelnut.Htyp.compare(t1, t2) == 0;

let htyp_print = (_: Hazelnut.Htyp.t): string => "htyp";

let htyp_typ = testable(Fmt.using(htyp_print, Fmt.string), htyp_eq);
