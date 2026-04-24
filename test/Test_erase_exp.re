open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

// Cursor erasure (Hazelnut 2017 Appendix A.2.2).
// For each rule, we use non-trivial sub-expressions (never a single Var/Lit)
// wherever the rule allows, because a common bug is to only erase at the
// "leaves" and leave inner cursors in place.

// EETop: .ė/ = ė  (the cursor is at the top)
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

// EETop with a large subject (checks that erase doesn't recurse past a top cursor).
let test_eetop_3 = () => {
  let ze: Hazelnut.Zexp.t =
    Cursor(
      Asc(
        Lam("f", Ap(Var("f"), Plus(Lit(1), Lit(2)))),
        Arrow(Arrow(Num, Num), Num),
      ),
    );
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Asc(
      Lam("f", Ap(Var("f"), Plus(Lit(1), Lit(2)))),
      Arrow(Arrow(Num, Num), Num),
    );
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// EEAscL: ê:τ̇ erased = (ê erased):τ̇
let test_eeascl_1 = () => {
  let ze: Hazelnut.Zexp.t =
    LAsc(Cursor(Lam("f", Lit(1))), Arrow(Num, Num));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Asc(Lam("f", Lit(1)), Arrow(Num, Num));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// Cursor inside the body of a lambda that is itself inside an ascription.
let test_eeascl_2 = () => {
  let ze: Hazelnut.Zexp.t =
    LAsc(Lam("f", Cursor(Lit(1))), Arrow(Num, Num));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Asc(Lam("f", Lit(1)), Arrow(Num, Num));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// EEAscL with a deeply nested compound subject (Plus inside Lam, inside LAsc).
let test_eeascl_3 = () => {
  let ze: Hazelnut.Zexp.t =
    LAsc(
      Lam("x", LPlus(Cursor(Var("x")), Lit(1))),
      Arrow(Num, Num),
    );
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Asc(Lam("x", Plus(Var("x"), Lit(1))), Arrow(Num, Num));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// EEAscR: ė:τ̂ erased = ė:(τ̂ erased)
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

// EEAscR with the type cursor nested inside an arrow (exercises ETArrL/ETArrR).
let test_eeascr_3 = () => {
  let ze: Hazelnut.Zexp.t =
    RAsc(
      Lam("f", Ap(Var("f"), Lit(1))),
      LArrow(RArrow(Num, Cursor(Num)), Num),
    );
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Asc(
      Lam("f", Ap(Var("f"), Lit(1))),
      Arrow(Arrow(Num, Num), Num),
    );
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// EELam: (λx.ê) erased = λx.(ê erased)
let test_eelam_1 = () => {
  let ze: Hazelnut.Zexp.t = Lam("f", Cursor(Lit(1)));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t = Lam("f", Lit(1));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// Cursor deeply nested inside a lambda body.
let test_eelam_2 = () => {
  let ze: Hazelnut.Zexp.t =
    Lam("f", LPlus(RPlus(Lit(1), Cursor(Lit(1))), Lit(2)));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Lam("f", Plus(Plus(Lit(1), Lit(1)), Lit(2)));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// EELam: nested lambdas with cursor in the inner body.
let test_eelam_3 = () => {
  let ze: Hazelnut.Zexp.t =
    Lam("f", Lam("g", Lam("h", Cursor(Ap(Var("g"), Lit(1))))));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Lam("f", Lam("g", Lam("h", Ap(Var("g"), Lit(1)))));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// EEApL: ê(ė) erased = (ê erased)(ė)
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

// EEApL where the RIGHT side is itself a compound expression (not a value).
// Verifies the right sibling is preserved intact without further erasure.
let test_eeapl_3 = () => {
  let ze: Hazelnut.Zexp.t =
    LAp(Cursor(Var("f")), Ap(Var("g"), Plus(Lit(1), Lit(2))));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Ap(Var("f"), Ap(Var("g"), Plus(Lit(1), Lit(2))));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// EEApR: ė(ê) erased = ė((ê erased))
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

// EEApR where the LEFT (non-cursor) side is a compound expression.
let test_eeapr_3 = () => {
  let ze: Hazelnut.Zexp.t =
    RAp(Asc(Lam("f", Var("f")), Arrow(Num, Num)), Cursor(Plus(Lit(1), Lit(2))));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Ap(Asc(Lam("f", Var("f")), Arrow(Num, Num)), Plus(Lit(1), Lit(2)));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// EEPlusL: (ê + ė) erased = ((ê erased) + ė)
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

// EEPlusL where both sides are compound — a canonical "Plus of Plusses".
let test_eeplusl_3 = () => {
  let ze: Hazelnut.Zexp.t =
    LPlus(LPlus(Cursor(Lit(2)), Lit(3)), Plus(Lit(4), Lit(5)));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Plus(Plus(Lit(2), Lit(3)), Plus(Lit(4), Lit(5)));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// EEPlusR: (ė + ê) erased = (ė + (ê erased))
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

// EEPlusR where the left side is a compound Plus expression.
let test_eeplusr_3 = () => {
  let ze: Hazelnut.Zexp.t =
    RPlus(Plus(Lit(2), Lit(3)), LPlus(Cursor(Lit(4)), Lit(5)));
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Plus(Plus(Lit(2), Lit(3)), Plus(Lit(4), Lit(5)));
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// EENEHole: ⦇ê⦈ erased = ⦇(ê erased)⦈
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

// EENEHole deeply nested in several different forms.
let test_eenehole_3 = () => {
  let ze: Hazelnut.Zexp.t =
    NEHole(
      LAsc(
        LPlus(Cursor(Ap(Var("f"), Lit(1))), Plus(Lit(2), Lit(3))),
        Num,
      ),
    );
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    NEHole(
      Asc(
        Plus(Ap(Var("f"), Lit(1)), Plus(Lit(2), Lit(3))),
        Num,
      ),
    );
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

// Mixed deep zipper: cursor buried several levels down through every
// non-leaf form in the grammar.
let test_ee_mixed_deep = () => {
  let ze: Hazelnut.Zexp.t =
    LAp(
      Lam(
        "f",
        RPlus(
          Ap(Var("f"), Lit(1)),
          NEHole(LAsc(Lam("x", Cursor(Var("x"))), Arrow(Num, Num))),
        ),
      ),
      Lit(2),
    );
  let given: Hazelnut.Hexp.t = Hazelnut.erase_exp(ze);
  let expected: Hazelnut.Hexp.t =
    Ap(
      Lam(
        "f",
        Plus(
          Ap(Var("f"), Lit(1)),
          NEHole(Asc(Lam("x", Var("x")), Arrow(Num, Num))),
        ),
      ),
      Lit(2),
    );
  check(hexp_typ, "same Hazelnut.Hexp.t", given, expected);
};

let erase_exp_tests = [
  ("test_eetop_1", `Quick, test_eetop_1),
  ("test_eetop_2", `Quick, test_eetop_2),
  ("test_eetop_3", `Quick, test_eetop_3),
  ("test_eeascl_1", `Quick, test_eeascl_1),
  ("test_eeascl_2", `Quick, test_eeascl_2),
  ("test_eeascl_3", `Quick, test_eeascl_3),
  ("test_eeascr_1", `Quick, test_eeascr_1),
  ("test_eeascr_2", `Quick, test_eeascr_2),
  ("test_eeascr_3", `Quick, test_eeascr_3),
  ("test_eelam_1", `Quick, test_eelam_1),
  ("test_eelam_2", `Quick, test_eelam_2),
  ("test_eelam_3", `Quick, test_eelam_3),
  ("test_eeapl_1", `Quick, test_eeapl_1),
  ("test_eeapl_2", `Quick, test_eeapl_2),
  ("test_eeapl_3", `Quick, test_eeapl_3),
  ("test_eeapr_1", `Quick, test_eeapr_1),
  ("test_eeapr_2", `Quick, test_eeapr_2),
  ("test_eeapr_3", `Quick, test_eeapr_3),
  ("test_eeplusl_1", `Quick, test_eeplusl_1),
  ("test_eeplusl_2", `Quick, test_eeplusl_2),
  ("test_eeplusl_3", `Quick, test_eeplusl_3),
  ("test_eeplusr_1", `Quick, test_eeplusr_1),
  ("test_eeplusr_2", `Quick, test_eeplusr_2),
  ("test_eeplusr_3", `Quick, test_eeplusr_3),
  ("test_eenehole_1", `Quick, test_eenehole_1),
  ("test_eenehole_2", `Quick, test_eenehole_2),
  ("test_eenehole_3", `Quick, test_eenehole_3),
  ("test_ee_mixed_deep", `Quick, test_ee_mixed_deep),
];
