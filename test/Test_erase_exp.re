open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

let test_eetop = () => {
  let ze: Hazelnut.Zexp.t = Cursor(Var("x"));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Var("x");
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeascl = () => {
  let ze: Hazelnut.Zexp.t =
    LAsc(Cursor(Lam("f", Lit(1))), Arrow(Num, Num));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Asc(Lam("f", Lit(1)), Arrow(Num, Num));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeascr = () => {
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("f", Lit(1)), Cursor(Arrow(Num, Num)));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Asc(Lam("f", Lit(1)), Arrow(Num, Num));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eelam = () => {
  let ze: Hazelnut.Zexp.t = Lam("f", Cursor(Lit(1)));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Lam("f", Lit(1));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeapl = () => {
  let ze: Hazelnut.Zexp.t = LAp(Cursor(Lam("f", Lit(1))), Var("x"));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Ap(Lam("f", Lit(1)), Var("x"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeapr = () => {
  let ze: Hazelnut.Zexp.t = RAp(Lam("f", Lit(1)), Cursor(Var("x")));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Ap(Lam("f", Lit(1)), Var("x"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeplusl = () => {
  let ze: Hazelnut.Zexp.t = LPlus(Cursor(Var("x")), Var("y"));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Plus(Var("x"), Var("y"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeplusr = () => {
  let ze: Hazelnut.Zexp.t = RPlus(Var("x"), Cursor(Var("y")));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Plus(Var("x"), Var("y"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eenehole = () => {
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lam("f", Lit(1))));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = NEHole(Lam("f", Lit(1)));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let erase_exp_tests = [
  ("test_eetop", `Quick, test_eetop),
  ("test_eeascl", `Quick, test_eeascl),
  ("test_eeascr", `Quick, test_eeascr),
  ("test_eelam", `Quick, test_eelam),
  ("test_eeapl", `Quick, test_eeapl),
  ("test_eeapr", `Quick, test_eeapr),
  ("test_eeplusl", `Quick, test_eeplusl),
  ("test_eeplusr", `Quick, test_eeplusr),
  ("test_eenehole", `Quick, test_eenehole),
];
