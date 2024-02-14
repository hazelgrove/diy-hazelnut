open Alcotest;
open Hazelnut_lib.Hazelnut;
open Test_interfaces;

let test_eetop = () => {
  let ze: zexp = Cursor(Var("x"));
  let given: hexp = erase_exp(ze);
  let expected: hexp = Var("x");
  check(hexp_typ, "same hexp", given, expected);
};

let test_eeascl = () => {
  let ze: zexp = LAsc(Cursor(Lam("f", Lit(1))), Arrow(Num, Num));
  let given: hexp = erase_exp(ze);
  let expected: hexp = Asc(Lam("f", Lit(1)), Arrow(Num, Num));
  check(hexp_typ, "same hexp", given, expected);
};

let test_eeascr = () => {
  let ze: zexp = RAsc(Lam("f", Lit(1)), Cursor(Arrow(Num, Num)));
  let given: hexp = erase_exp(ze);
  let expected: hexp = Asc(Lam("f", Lit(1)), Arrow(Num, Num));
  check(hexp_typ, "same hexp", given, expected);
};

let test_eelam = () => {
  let ze: zexp = Lam("f", Cursor(Lit(1)));
  let given: hexp = erase_exp(ze);
  let expected: hexp = Lam("f", Lit(1));
  check(hexp_typ, "same hexp", given, expected);
};

let test_eeapl = () => {
  let ze: zexp = LAp(Cursor(Lam("f", Lit(1))), Var("x"));
  let given: hexp = erase_exp(ze);
  let expected: hexp = Ap(Lam("f", Lit(1)), Var("x"));
  check(hexp_typ, "same hexp", given, expected);
};

let test_eeapr = () => {
  let ze: zexp = RAp(Lam("f", Lit(1)), Cursor(Var("x")));
  let given: hexp = erase_exp(ze);
  let expected: hexp = Ap(Lam("f", Lit(1)), Var("x"));
  check(hexp_typ, "same hexp", given, expected);
};

let test_eeplusl = () => {
  let ze: zexp = LPlus(Cursor(Var("x")), Var("y"));
  let given: hexp = erase_exp(ze);
  let expected: hexp = Plus(Var("x"), Var("y"));
  check(hexp_typ, "same hexp", given, expected);
};

let test_eeplusr = () => {
  let ze: zexp = RPlus(Var("x"), Cursor(Var("y")));
  let given: hexp = erase_exp(ze);
  let expected: hexp = Plus(Var("x"), Var("y"));
  check(hexp_typ, "same hexp", given, expected);
};

let test_eenehole = () => {
  let ze: zexp = NEHole(Cursor(Lam("f", Lit(1))));
  let given: hexp = erase_exp(ze);
  let expected: hexp = NEHole(Lam("f", Lit(1)));
  check(hexp_typ, "same hexp", given, expected);
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
