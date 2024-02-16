open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

let test_eetop_1 = () => {
  let ze: Hazelnut.Zexp.t = Cursor(Var("x"));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Var("x");
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eetop_2 = () => {
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = EHole;
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeascl_1 = () => {
  let ze: Hazelnut.Zexp.t =
    LAsc(Cursor(Lam("f", Lit(1))), Arrow(Num, Num));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Asc(Lam("f", Lit(1)), Arrow(Num, Num));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeascl_2 = () => {
  let ze: Hazelnut.Zexp.t =
    LAsc(Lam("f", Cursor(Lit(1))), Arrow(Num, Num));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Asc(Lam("f", Lit(1)), Arrow(Num, Num));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeascr_1 = () => {
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("f", Lit(1)), Cursor(Arrow(Num, Num)));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Asc(Lam("f", Lit(1)), Arrow(Num, Num));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeascr_2 = () => {
  let ze: Hazelnut.Zexp.t = RAsc(NEHole(Var("y")), Cursor(Hole));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Asc(NEHole(Var("y")), Hole);
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eelam_1 = () => {
  let ze: Hazelnut.Zexp.t = Lam("f", Cursor(Lit(1)));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Lam("f", Lit(1));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eelam_2 = () => {
  let ze: Hazelnut.Zexp.t =
    Lam("f", LPlus(RPlus(Lit(1), Cursor(Lit(1))), Lit(2)));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Lam("f", Plus(Plus(Lit(1), Lit(1)), Lit(2)));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeapl_1 = () => {
  let ze: Hazelnut.Zexp.t = LAp(Cursor(Lam("f", Lit(1))), Var("x"));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Ap(Lam("f", Lit(1)), Var("x"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeapl_2 = () => {
  let ze: Hazelnut.Zexp.t =
    LAp(Lam("f", Lam("g", Cursor(EHole))), Var("x"));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Ap(Lam("f", Lam("g", EHole)), Var("x"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeapr_1 = () => {
  let ze: Hazelnut.Zexp.t = RAp(Lam("f", Lit(1)), Cursor(Var("x")));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Ap(Lam("f", Lit(1)), Var("x"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeapr_2 = () => {
  let ze: Hazelnut.Zexp.t =
    RAp(Lam("f", Lit(1)), LAsc(NEHole(Cursor(Lit(1))), Hole));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Ap(Lam("f", Lit(1)), Asc(NEHole(Lit(1)), Hole));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeplusl_1 = () => {
  let ze: Hazelnut.Zexp.t = LPlus(Cursor(Var("x")), Var("y"));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Plus(Var("x"), Var("y"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeplusl_2 = () => {
  let ze: Hazelnut.Zexp.t =
    LPlus(Lam("f", RPlus(Lit(1), Cursor(Lit(2)))), Var("y"));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Plus(Lam("f", Plus(Lit(1), Lit(2))), Var("y"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeplusr_1 = () => {
  let ze: Hazelnut.Zexp.t = RPlus(Var("x"), Cursor(Var("y")));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Plus(Var("x"), Var("y"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeplusr_2 = () => {
  let ze: Hazelnut.Zexp.t =
    RPlus(Var("x"), NEHole(NEHole(Cursor(Var("y")))));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Plus(Var("x"), NEHole(NEHole(Var("y"))));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eenehole_1 = () => {
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lam("f", Lit(1))));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = NEHole(Lam("f", Lit(1)));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eenehole_2 = () => {
  let ze: Hazelnut.Zexp.t =
    NEHole(LAp(NEHole(Cursor(Var("f"))), Var("x")));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = NEHole(Ap(NEHole(Var("f")), Var("x")));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let erase_exp_tests = [
  ("test_eetop_1", `Quick, test_eetop_1),
  ("test_eetop_2", `Quick, test_eetop_2),
  ("test_eeascl_1", `Quick, test_eeascl_1),
  ("test_eeascl_2", `Quick, test_eeascl_2),
  ("test_eeascr_1", `Quick, test_eeascr_1),
  ("test_eeascr_2", `Quick, test_eeascr_2),
  ("test_eelam_1", `Quick, test_eelam_1),
  ("test_eelam_2", `Quick, test_eelam_2),
  ("test_eeapl_1", `Quick, test_eeapl_1),
  ("test_eeapl_2", `Quick, test_eeapl_2),
  ("test_eeapr_1", `Quick, test_eeapr_1),
  ("test_eeapr_2", `Quick, test_eeapr_2),
  ("test_eeplusl_1", `Quick, test_eeplusl_1),
  ("test_eeplusl_2", `Quick, test_eeplusl_2),
  ("test_eeplusr_1", `Quick, test_eeplusr_1),
  ("test_eeplusr_2", `Quick, test_eeplusr_2),
  ("test_eenehole_1", `Quick, test_eenehole_1),
  ("test_eenehole_2", `Quick, test_eenehole_2),
];
