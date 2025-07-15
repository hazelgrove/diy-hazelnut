open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

let test_st14 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("incr"));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((Cursor(Var("incr")), Arrow(Num, Num)));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ast14 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("incr"));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) = Some(Cursor(Var("incr")));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st15 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(Var("incr"));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RAp(Var("incr"), Cursor(EHole)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ast15 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(Var("incr"));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAp(Var("incr"), Cursor(EHole)));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st16 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = RAp(Var("incr"), Cursor(EHole));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("incr"));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RAp(Var("incr"), NEHole(Cursor(Var("incr")))), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ast16 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = RAp(Var("incr"), Cursor(EHole));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("incr"));
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAp(Var("incr"), NEHole(Cursor(Var("incr")))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st17 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = RAp(Var("incr"), NEHole(Cursor(Var("incr"))));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAp(Var("incr"), NEHole(RAp(Var("incr"), Cursor(EHole)))),
      Num,
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ast17 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = RAp(Var("incr"), NEHole(Cursor(Var("incr"))));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAp(Var("incr"), NEHole(RAp(Var("incr"), Cursor(EHole)))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st18 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    RAp(Var("incr"), NEHole(RAp(Var("incr"), Cursor(EHole))));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lit(3));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAp(Var("incr"), NEHole(RAp(Var("incr"), Cursor(Lit(3))))),
      Num,
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ast18 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    RAp(Var("incr"), NEHole(RAp(Var("incr"), Cursor(EHole))));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lit(3));
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAp(Var("incr"), NEHole(RAp(Var("incr"), Cursor(Lit(3))))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st19 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    RAp(Var("incr"), NEHole(RAp(Var("incr"), Cursor(Lit(3)))));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAp(Var("incr"), NEHole(Cursor(Ap(Var("incr"), Lit(3))))),
      Num,
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ast19 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    RAp(Var("incr"), NEHole(RAp(Var("incr"), Cursor(Lit(3)))));
  let a: Hazelnut.Action.t = Move(Parent);
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAp(Var("incr"), NEHole(Cursor(Ap(Var("incr"), Lit(3))))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st20 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    RAp(Var("incr"), NEHole(Cursor(Ap(Var("incr"), Lit(3)))));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAp(Var("incr"), Cursor(NEHole(Ap(Var("incr"), Lit(3))))),
      Num,
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ast20 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    RAp(Var("incr"), NEHole(Cursor(Ap(Var("incr"), Lit(3)))));
  let a: Hazelnut.Action.t = Move(Parent);
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAp(Var("incr"), Cursor(NEHole(Ap(Var("incr"), Lit(3))))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st21 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    RAp(Var("incr"), Cursor(NEHole(Ap(Var("incr"), Lit(3)))));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Finish;
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RAp(Var("incr"), Cursor(Ap(Var("incr"), Lit(3)))), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ast21 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    RAp(Var("incr"), Cursor(NEHole(Ap(Var("incr"), Lit(3)))));
  let a: Hazelnut.Action.t = Finish;
  let ht: Hazelnut.Htyp.t = Num;
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(RAp(Var("incr"), Cursor(Ap(Var("incr"), Lit(3)))));
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let sample2_tests = [
  ("test_syn_step_14", `Quick, test_st14),
  ("test_ana_step_14", `Quick, test_ast14),
  ("test_syn_step_15", `Quick, test_st15),
  ("test_ana_step_15", `Quick, test_ast15),
  ("test_syn_step_16", `Quick, test_st16),
  ("test_ana_step_16", `Quick, test_ast16),
  ("test_syn_step_17", `Quick, test_st17),
  ("test_ana_step_17", `Quick, test_ast17),
  ("test_syn_step_18", `Quick, test_st18),
  ("test_ana_step_18", `Quick, test_ast18),
  ("test_syn_step_19", `Quick, test_st19),
  ("test_ana_step_19", `Quick, test_ast19),
  ("test_syn_step_20", `Quick, test_st20),
  ("test_ana_step_20", `Quick, test_ast20),
  ("test_syn_step_21", `Quick, test_st21),
  ("test_ana_step_21", `Quick, test_ast21),
];
