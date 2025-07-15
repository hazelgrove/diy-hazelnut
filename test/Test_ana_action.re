open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

let test_aamove_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lam("f", Plus(Lit(1), Lit(2))));
  let a: Hazelnut.Action.t = Move(Child(One));
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(Lam("f", Cursor(Plus(Lit(1), Lit(2)))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aamove_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = Cursor(Ap(Lam("f", Lit(1)), Var("x")));
  let a: Hazelnut.Action.t = Move(Child(Two));
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAp(Lam("f", Lit(1)), Cursor(Var("x"))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aamove_3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("f", Lit(1)), Cursor(Arrow(Num, Num)));
  let a: Hazelnut.Action.t = Move(Parent);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(Cursor(Asc(Lam("f", Lit(1)), Arrow(Num, Num))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aadel_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("f", Lit(1)), Cursor(Arrow(Num, Num)));
  let a: Hazelnut.Action.t = Del;
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAsc(Lam("f", Lit(1)), Cursor(Hole)));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aadel_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("f", Plus(Lit(1), Var("x"))), RArrow(Num, Cursor(Num)));
  let a: Hazelnut.Action.t = Del;
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      RAsc(Lam("f", Plus(Lit(1), Var("x"))), RArrow(Num, Cursor(Hole))),
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aaconasc_1 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = Cursor(Var("x"));
  let a: Hazelnut.Action.t = Construct(Asc);
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAsc(Var("x"), Cursor(Num)));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aaconasc_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(Var("x"));
  let a: Hazelnut.Action.t = Construct(Asc);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAsc(Var("x"), Cursor(Arrow(Num, Num))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aaconvar_1 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Var("x"));
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) = Some(NEHole(Cursor(Var("x"))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aaconvar_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Var("x"));
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) = Some(Cursor(Var("x")));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aaconlam1_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lam("x"));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) = Some(Lam("x", Cursor(EHole)));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aaconlam1_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lam("x"));
  let ht: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) = Some(Lam("x", Cursor(EHole)));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aaconlam2_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lam("x"));
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(NEHole(RAsc(Lam("x", EHole), LArrow(Cursor(Hole), Hole))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aaconlam2_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lam("y"));
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(NEHole(RAsc(Lam("y", EHole), LArrow(Cursor(Hole), Hole))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aaconnumlit_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lit(1));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) = Some(NEHole(Cursor(Lit(1))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aaconnumlit_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lit(-1));
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) = Some(Cursor(Lit(-1)));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aafinish_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(NEHole(Plus(Lit(1), Lit(1))));
  let a: Hazelnut.Action.t = Finish;
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(Cursor(Plus(Lit(1), Lit(1))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_aafinish_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t =
    Cursor(NEHole(NEHole(Lam("f", Plus(Var("x"), Var("x"))))));
  let a: Hazelnut.Action.t = Finish;
  let ht: Hazelnut.Htyp.t = Hole;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) = None;
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let ana_action_tests = [
  ("test_aamove_1", `Quick, test_aamove_1),
  ("test_aamove_2", `Quick, test_aamove_2),
  ("test_aamove_3", `Quick, test_aamove_3),
  ("test_aadel_1", `Quick, test_aadel_1),
  ("test_aadel_2", `Quick, test_aadel_2),
  ("test_aaconasc_1", `Quick, test_aaconasc_1),
  ("test_aaconasc_2", `Quick, test_aaconasc_2),
  ("test_aaconvar_1", `Quick, test_aaconvar_1),
  ("test_aaconvar_2", `Quick, test_aaconvar_2),
  ("test_aaconlam1_1", `Quick, test_aaconlam1_1),
  ("test_aaconlam1_2", `Quick, test_aaconlam1_2),
  ("test_aaconlam2_1", `Quick, test_aaconlam2_1),
  ("test_aaconlam2_2", `Quick, test_aaconlam2_2),
  ("test_aaconnumlit_1", `Quick, test_aaconnumlit_1),
  ("test_aaconnumlit_2", `Quick, test_aaconnumlit_2),
  ("test_aafinish_1", `Quick, test_aafinish_1),
  ("test_aafinish_2", `Quick, test_aafinish_2),
];
