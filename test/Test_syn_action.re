open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;
module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

// MOVE TESTS BEGIN
let test_samove_asc1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Asc(Lit(1), Num));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Child(One));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((LAsc(Cursor(Lit(1)), Num), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_asc2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Asc(Lit(1), Num));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Child(Two));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RAsc(Lit(1), Cursor(Num)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_asc3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = LAsc(Cursor(Lit(1)), Num);
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((Cursor(Asc(Lit(1), Num)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_asc4 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAsc(Lit(1), Cursor(Num));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((Cursor(Asc(Lit(1), Num)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_lam1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    LAsc(Cursor(Lam("x", EHole)), Arrow(Hole, Hole));
  let t: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let a: Hazelnut.Action.t = Move(Child(One));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      LAsc(Lam("x", Cursor(EHole)), Arrow(Hole, Hole)),
      Arrow(Hole, Hole),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_lam2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    LAsc(Lam("x", Cursor(EHole)), Arrow(Hole, Hole));
  let t: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      LAsc(Cursor(Lam("x", EHole)), Arrow(Hole, Hole)),
      Arrow(Hole, Hole),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_plus1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Plus(Lit(1), Lit(2)));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Child(One));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((LPlus(Cursor(Lit(1)), Lit(2)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_plus2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Plus(Lit(1), Lit(2)));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Child(Two));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RPlus(Lit(1), Cursor(Lit(2))), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_plus3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = LPlus(Cursor(Lit(1)), Lit(2));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((Cursor(Plus(Lit(1), Lit(2))), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_plus4 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RPlus(Lit(1), Cursor(Lit(2)));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((Cursor(Plus(Lit(1), Lit(2))), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_ap1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Ap(Lam("x", EHole), EHole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Move(Child(One));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((LAp(Cursor(Lam("x", EHole)), EHole), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_ap2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Ap(Lam("x", EHole), EHole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Move(Child(Two));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RAp(Lam("x", EHole), Cursor(EHole)), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_ap3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = LAp(Cursor(Lam("x", EHole)), EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((Cursor(Ap(Lam("x", EHole), EHole)), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_ap4 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAp(Lam("x", EHole), Cursor(EHole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((Cursor(Ap(Lam("x", EHole), EHole)), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_neh1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(NEHole(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Move(Child(One));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((NEHole(Cursor(Lit(1))), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_samove_neh2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((Cursor(NEHole(Lit(1))), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
// MOVE TESTS COMPLETE

let test_sadel_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Del;
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((Cursor(EHole), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_sadel_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Del;
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((NEHole(Cursor(EHole)), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconasc_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Asc);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RAsc(Lit(1), Cursor(Num)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconasc_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Asc);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RAsc(EHole, Cursor(Hole)), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconasc_3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Asc);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((NEHole(RAsc(Lit(1), Cursor(Num))), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconvar_1 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("x"));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((Cursor(Var("x")), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconvar_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(EHole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("x"));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((NEHole(Cursor(Var("x"))), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconlam_1 = () => {
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
let test_saconlam_2 = () => {
  //CHECK THIS ONE
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(EHole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lam("x"));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      NEHole(RAsc(Lam("x", EHole), LArrow(Cursor(Hole), Hole))),
      Hole,
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconnumlit_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lit(1));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((Cursor(Lit(1)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconnumlit_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(EHole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lit(1));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((NEHole(Cursor(Lit(1))), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconnehole_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.NEHole);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((NEHole(Cursor(Lit(1))), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconnehole_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.NEHole);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((NEHole(NEHole(Cursor(Lit(1)))), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconaparr_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lam("x", EHole));
  let t: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RAp(Lam("x", EHole), Cursor(EHole)), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconapotw_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RAp(NEHole(Lit(1)), Cursor(EHole)), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconapotw_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((NEHole(RAp(NEHole(Lit(1)), Cursor(EHole))), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconplus1_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RPlus(Lit(1), Cursor(EHole)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconplus1_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((NEHole(RPlus(Lit(1), Cursor(EHole))), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_saconplus2_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lam("x", EHole));
  let t: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RPlus(NEHole(Lam("x", EHole)), Cursor(EHole)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_safinish_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(NEHole(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Finish;
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((Cursor(Lit(1)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_safinish_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(NEHole(Lit(1))));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Finish;
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((NEHole(Cursor(Lit(1))), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let syn_action_tests = [
  ("test_samove_asc1", `Quick, test_samove_asc1),
  ("test_samove_asc2", `Quick, test_samove_asc2),
  ("test_samove_asc3", `Quick, test_samove_asc3),
  ("test_samove_asc4", `Quick, test_samove_asc4),
  ("test_samove_lam1", `Quick, test_samove_lam1),
  ("test_samove_lam2", `Quick, test_samove_lam2),
  ("test_samove_plus1", `Quick, test_samove_plus1),
  ("test_samove_plus2", `Quick, test_samove_plus2),
  ("test_samove_plus3", `Quick, test_samove_plus3),
  ("test_samove_plus4", `Quick, test_samove_plus4),
  ("test_samove_ap1", `Quick, test_samove_ap1),
  ("test_samove_ap2", `Quick, test_samove_ap2),
  ("test_samove_ap3", `Quick, test_samove_ap3),
  ("test_samove_ap4", `Quick, test_samove_ap4),
  ("test_samove_neh1", `Quick, test_samove_neh1),
  ("test_samove_neh2", `Quick, test_samove_neh2),
  ("test_sadel_1", `Quick, test_sadel_1),
  ("test_sadel_2", `Quick, test_sadel_2),
  ("test_safinish_1", `Quick, test_safinish_1),
  ("test_safinish_2", `Quick, test_safinish_2),
  ("test_saconasc_1", `Quick, test_saconasc_1),
  ("test_saconasc_2", `Quick, test_saconasc_2),
  ("test_saconasc_3", `Quick, test_saconasc_3),
  ("test_saconvar_1", `Quick, test_saconvar_1),
  ("test_saconvar_2", `Quick, test_saconvar_2),
  ("test_saconlam_1", `Quick, test_saconlam_1),
  ("test_saconlam_2", `Quick, test_saconlam_2),
  ("test_saconnumlit_1", `Quick, test_saconnumlit_1),
  ("test_saconnumlit_2", `Quick, test_saconnumlit_2),
  ("test_saconnehole_1", `Quick, test_saconnehole_1),
  ("test_saconnehole_2", `Quick, test_saconnehole_2),
  ("test_saconaparr_1", `Quick, test_saconaparr_1),
  ("test_saconapotw_1", `Quick, test_saconapotw_1),
  ("test_saconapotw_2", `Quick, test_saconapotw_2),
  ("test_saconplus1_1", `Quick, test_saconplus1_1),
  ("test_saconplus1_2", `Quick, test_saconplus1_2),
  ("test_saconplus2_1", `Quick, test_saconplus2_1),
];
