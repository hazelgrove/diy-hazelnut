module Hazelnut = Hazelnut_lib.Hazelnut;
open Alcotest;
open Test_interface;

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
    LAsc(Cursor(Lam("f", Num, NumLit(1))), Arrow(Num, Num));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Asc(Lam("f", Num, NumLit(1)), Arrow(Num, Num));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeascl_2 = () => {
  let ze: Hazelnut.Zexp.t =
    LAsc(RLam("f", Num, Cursor(NumLit(1))), Arrow(Num, Num));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Asc(Lam("f", Num, NumLit(1)), Arrow(Num, Num));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeascr_1 = () => {
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("f", Num, NumLit(1)), Cursor(Arrow(Num, Num)));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Asc(Lam("f", Num, NumLit(1)), Arrow(Num, Num));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeascr_2 = () => {
  let ze: Hazelnut.Zexp.t = RAsc(Mark(Var("y"), Free), Cursor(Hole));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Asc(Mark(Var("y"), Free), Hole);
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eelam_1 = () => {
  let ze: Hazelnut.Zexp.t = RLam("f", Num, Cursor(NumLit(1)));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Lam("f", Num, NumLit(1));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eelam_2 = () => {
  let ze: Hazelnut.Zexp.t =
    RLam(
      "f",
      Num,
      LPlus(RPlus(NumLit(1), Cursor(NumLit(1))), NumLit(2)),
    );
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Lam("f", Num, Plus(Plus(NumLit(1), NumLit(1)), NumLit(2)));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeapl_1 = () => {
  let ze: Hazelnut.Zexp.t =
    LAp(Cursor(Lam("f", Num, NumLit(1))), Var("x"));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Ap(Lam("f", Num, NumLit(1)), Var("x"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeapl_2 = () => {
  let ze: Hazelnut.Zexp.t =
    LAp(RLam("f", Num, RLam("g", Hole, Cursor(EHole))), Var("x"));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Ap(Lam("f", Num, Lam("g", Hole, EHole)), Var("x"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeapr_1 = () => {
  let ze: Hazelnut.Zexp.t =
    RAp(Lam("f", Num, NumLit(1)), Cursor(Var("x")));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Ap(Lam("f", Num, NumLit(1)), Var("x"));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eeapr_2 = () => {
  let ze: Hazelnut.Zexp.t =
    RAp(
      Lam("f", Num, NumLit(1)),
      LAsc(Mark(Cursor(NumLit(1)), Free), Hole),
    ); // not real state, just test
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Ap(Lam("f", Num, NumLit(1)), Asc(Mark(NumLit(1), Free), Hole));
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
    LPlus(
      RLam("f", Num, RPlus(NumLit(1), Cursor(NumLit(2)))),
      Var("y"),
    );
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Plus(Lam("f", Num, Plus(NumLit(1), NumLit(2))), Var("y"));
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
    RPlus(Var("x"), Mark(Mark(Cursor(Var("y")), Free), NonArrowAp));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Plus(Var("x"), Mark(Mark(Var("y"), Free), NonArrowAp));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eemark_1 = () => {
  let ze: Hazelnut.Zexp.t = Mark(Cursor(Lam("f", Num, NumLit(1))), Free);
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Mark(Lam("f", Num, NumLit(1)), Free);
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let test_eemark_2 = () => {
  let ze: Hazelnut.Zexp.t =
    Mark(LAp(Mark(Cursor(Var("f")), Free), Var("x")), NonArrowAp);
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Mark(Ap(Mark(Var("f"), Free), Var("x")), NonArrowAp);
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
  ("test_eemark_1", `Quick, test_eemark_1),
  ("test_eemark_2", `Quick, test_eemark_2),
];
