open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

let test_syn_step_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lam("x"));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(Lam("x", EHole), LArrow(Cursor(Hole), Hole)),
      Arrow(Hole, Hole),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ana_step_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lam("x"));
  let ht: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) = Some(Lam("x", Cursor(EHole)));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_syn_step_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), LArrow(Cursor(Hole), Hole));
  let t: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Num);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(Lam("x", EHole), LArrow(Cursor(Num), Hole)),
      Arrow(Num, Hole),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ana_step_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), LArrow(Cursor(Hole), Hole));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Num);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Hole);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAsc(Lam("x", EHole), LArrow(Cursor(Num), Hole)));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_syn_step_3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), LArrow(Cursor(Num), Hole));
  let t: Hazelnut.Htyp.t = Arrow(Num, Hole);
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(Lam("x", EHole), Cursor(Arrow(Num, Hole))),
      Arrow(Num, Hole),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ana_step_3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), LArrow(Cursor(Num), Hole));
  let a: Hazelnut.Action.t = Move(Parent);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Hole);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAsc(Lam("x", EHole), Cursor(Arrow(Num, Hole))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_syn_step_4 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), Cursor(Arrow(Num, Hole)));
  let t: Hazelnut.Htyp.t = Arrow(Num, Hole);
  let a: Hazelnut.Action.t = Move(Child(Two));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(Lam("x", EHole), RArrow(Num, Cursor(Hole))),
      Arrow(Num, Hole),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ana_step_4 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), Cursor(Arrow(Num, Hole)));
  let a: Hazelnut.Action.t = Move(Child(Two));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Hole);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAsc(Lam("x", EHole), RArrow(Num, Cursor(Hole))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_syn_step_5 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), RArrow(Num, Cursor(Hole)));
  let t: Hazelnut.Htyp.t = Arrow(Num, Hole);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Num);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(Lam("x", EHole), RArrow(Num, Cursor(Num))),
      Arrow(Num, Num),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ana_step_5 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), RArrow(Num, Cursor(Hole)));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Num);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAsc(Lam("x", EHole), RArrow(Num, Cursor(Num))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_syn_step_6 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), RArrow(Num, Cursor(Num)));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(Lam("x", EHole), Cursor(Arrow(Num, Num))),
      Arrow(Num, Num),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ana_step_6 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), RArrow(Num, Cursor(Num)));
  let a: Hazelnut.Action.t = Move(Parent);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAsc(Lam("x", EHole), Cursor(Arrow(Num, Num))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_syn_step_7 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAsc(Lam("x", EHole), Cursor(Arrow(Num, Num)));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      Cursor(Asc(Lam("x", EHole), Arrow(Num, Num))),
      Arrow(Num, Num),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ana_step_7 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAsc(Lam("x", EHole), Cursor(Arrow(Num, Num)));
  let a: Hazelnut.Action.t = Move(Parent);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(Cursor(Asc(Lam("x", EHole), Arrow(Num, Num))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_syn_step_8 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Asc(Lam("x", EHole), Arrow(Num, Num)));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Move(Child(One));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      LAsc(Cursor(Lam("x", EHole)), Arrow(Num, Num)),
      Arrow(Num, Num),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ana_step_8 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Asc(Lam("x", EHole), Arrow(Num, Num)));
  let a: Hazelnut.Action.t = Move(Child(One));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(LAsc(Cursor(Lam("x", EHole)), Arrow(Num, Num)));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_syn_step_9 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = LAsc(Cursor(Lam("x", EHole)), Arrow(Num, Num));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Move(Child(One));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      LAsc(Lam("x", Cursor(EHole)), Arrow(Num, Num)),
      Arrow(Num, Num),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ana_step_9 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = LAsc(Cursor(Lam("x", EHole)), Arrow(Num, Num));
  let a: Hazelnut.Action.t = Move(Child(One));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(LAsc(Lam("x", Cursor(EHole)), Arrow(Num, Num)));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_syn_step_10 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = LAsc(Lam("x", Cursor(EHole)), Arrow(Num, Num));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("x"));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      LAsc(Lam("x", Cursor(Var("x"))), Arrow(Num, Num)),
      Arrow(Num, Num),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ana_step_10 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = LAsc(Lam("x", Cursor(EHole)), Arrow(Num, Num));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("x"));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(LAsc(Lam("x", Cursor(Var("x"))), Arrow(Num, Num)));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_syn_step_11 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t =
    LAsc(Lam("x", Cursor(Var("x"))), Arrow(Num, Num));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      LAsc(Lam("x", RPlus(Var("x"), Cursor(EHole))), Arrow(Num, Num)),
      Arrow(Num, Num),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ana_step_11 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t =
    LAsc(Lam("x", Cursor(Var("x"))), Arrow(Num, Num));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      LAsc(Lam("x", RPlus(Var("x"), Cursor(EHole))), Arrow(Num, Num)),
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_syn_step_12 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t =
    LAsc(Lam("x", RPlus(Var("x"), Cursor(EHole))), Arrow(Num, Num));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lit(1));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      LAsc(Lam("x", RPlus(Var("x"), Cursor(Lit(1)))), Arrow(Num, Num)),
      Arrow(Num, Num),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ana_step_12 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t =
    LAsc(Lam("x", RPlus(Var("x"), Cursor(EHole))), Arrow(Num, Num));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lit(1));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      LAsc(Lam("x", RPlus(Var("x"), Cursor(Lit(1)))), Arrow(Num, Num)),
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let sample1_tests = [
  ("test_syn_step_1", `Quick, test_syn_step_1),
  ("test_ana_step_1", `Quick, test_ana_step_1),
  ("test_syn_step_2", `Quick, test_syn_step_2),
  ("test_ana_step_2", `Quick, test_ana_step_2),
  ("test_syn_step_3", `Quick, test_syn_step_3),
  ("test_ana_step_3", `Quick, test_ana_step_3),
  ("test_syn_step_4", `Quick, test_syn_step_4),
  ("test_ana_step_4", `Quick, test_ana_step_4),
  ("test_syn_step_5", `Quick, test_syn_step_5),
  ("test_ana_step_5", `Quick, test_ana_step_5),
  ("test_syn_step_6", `Quick, test_syn_step_6),
  ("test_ana_step_6", `Quick, test_ana_step_6),
  ("test_syn_step_7", `Quick, test_syn_step_7),
  ("test_ana_step_7", `Quick, test_ana_step_7),
  ("test_syn_step_8", `Quick, test_syn_step_8),
  ("test_ana_step_8", `Quick, test_ana_step_8),
  ("test_syn_step_9", `Quick, test_syn_step_9),
  ("test_ana_step_9", `Quick, test_ana_step_9),
  ("test_syn_step_10", `Quick, test_syn_step_10),
  ("test_ana_step_10", `Quick, test_ana_step_10),
  ("test_syn_step_11", `Quick, test_syn_step_11),
  ("test_ana_step_11", `Quick, test_ana_step_11),
  ("test_syn_step_12", `Quick, test_syn_step_12),
  ("test_ana_step_12", `Quick, test_ana_step_12),
];
