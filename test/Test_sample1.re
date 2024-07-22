open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

let test_st1 = () => {
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

let test_ast1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lam("x"));
  let ht: Hazelnut.Htyp.t = Arrow(Hole,Hole);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      Lam("x", Cursor(EHole))
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st2 = () => {
    let ctx: typctx = TypCtx.empty;
    let ze: Hazelnut.Zexp.t = RAsc(Lam("x", EHole), LArrow(Cursor(Hole), Hole));
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

let test_ast2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAsc(Lam("x", EHole), LArrow(Cursor(Hole), Hole));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Num);
  let ht: Hazelnut.Htyp.t = Arrow(Num,Hole);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      RAsc(Lam("x", EHole), LArrow(Cursor(Num), Hole))
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st3 = () => {
    let ctx: typctx = TypCtx.empty;
    let ze: Hazelnut.Zexp.t = RAsc(Lam("x", EHole), LArrow(Cursor(Num), Hole));
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

let test_ast3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAsc(Lam("x", EHole), LArrow(Cursor(Num), Hole));
  let a: Hazelnut.Action.t = Move(Parent);
  let ht: Hazelnut.Htyp.t = Arrow(Num,Hole);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      RAsc(Lam("x", EHole), Cursor(Arrow(Num, Hole)))
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st4 = () => {
    let ctx: typctx = TypCtx.empty;
    let ze: Hazelnut.Zexp.t =  RAsc(Lam("x", EHole), Cursor(Arrow(Num, Hole)));
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

let test_ast4 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAsc(Lam("x", EHole), Cursor(Arrow(Num, Hole)));
  let a: Hazelnut.Action.t = Move(Child(Two));
  let ht: Hazelnut.Htyp.t = Arrow(Num,Hole);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      RAsc(Lam("x", EHole), RArrow(Num, Cursor(Hole)))
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st5 = () => {
    let ctx: typctx = TypCtx.empty;
    let ze: Hazelnut.Zexp.t =  RAsc(Lam("x", EHole), RArrow(Num, Cursor(Hole)));
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

let test_ast5 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =  RAsc(Lam("x", EHole), RArrow(Num, Cursor(Hole)));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Num);
  let ht: Hazelnut.Htyp.t = Arrow(Num,Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      RAsc(Lam("x", EHole), RArrow(Num, Cursor(Num)))
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st6 = () => {
    let ctx: typctx = TypCtx.empty;
    let ze: Hazelnut.Zexp.t =  RAsc(Lam("x", EHole), RArrow(Num, Cursor(Num)));
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

let test_ast6 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =  RAsc(Lam("x", EHole), RArrow(Num, Cursor(Num)));
  let a: Hazelnut.Action.t = Move(Parent);
  let ht: Hazelnut.Htyp.t = Arrow(Num,Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      RAsc(Lam("x", EHole), Cursor(Arrow(Num, Num)))
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st7 = () => {
    let ctx: typctx = TypCtx.empty;
    let ze: Hazelnut.Zexp.t =  RAsc(Lam("x", EHole), Cursor(Arrow(Num, Num)));
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

let test_ast7 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =  RAsc(Lam("x", EHole), Cursor(Arrow(Num, Num)));
  let a: Hazelnut.Action.t = Move(Parent);
  let ht: Hazelnut.Htyp.t = Arrow(Num,Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      Cursor(Asc(Lam("x", EHole), Arrow(Num, Num)))
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st8 = () => {
    let ctx: typctx = TypCtx.empty;
    let ze: Hazelnut.Zexp.t =  Cursor(Asc(Lam("x", EHole), Arrow(Num, Num)));
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

let test_ast8 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =  Cursor(Asc(Lam("x", EHole), Arrow(Num, Num)));
  let a: Hazelnut.Action.t = Move(Child(One));
  let ht: Hazelnut.Htyp.t = Arrow(Num,Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      LAsc(Cursor(Lam("x", EHole)), Arrow(Num, Num))
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st9 = () => {
    let ctx: typctx = TypCtx.empty;
    let ze: Hazelnut.Zexp.t =  LAsc(Cursor(Lam("x", EHole)), Arrow(Num, Num));
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

let test_ast9 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =  LAsc(Cursor(Lam("x", EHole)), Arrow(Num, Num));
  let a: Hazelnut.Action.t = Move(Child(One));
  let ht: Hazelnut.Htyp.t = Arrow(Num,Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      LAsc(Lam("x", Cursor(EHole)), Arrow(Num, Num))
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st10 = () => {
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

let test_ast10 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t =  LAsc(Lam("x", Cursor(EHole)), Arrow(Num, Num));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("x"));
  let ht: Hazelnut.Htyp.t = Arrow(Num,Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      LAsc(Lam("x", Cursor(Var("x"))), Arrow(Num, Num))
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st11 = () => {
    let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
    let ze: Hazelnut.Zexp.t = LAsc(Lam("x", Cursor(Var("x"))), Arrow(Num, Num));
    let t: Hazelnut.Htyp.t = Arrow(Num, Num);
    let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
    let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
      Hazelnut.syn_action(ctx, (ze, t), a);
    let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
      Some((
        LAsc(Lam("x", RPlus(Var("x"),Cursor(EHole))), Arrow(Num, Num)),
        Arrow(Num, Num),
      ));
    check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ast11 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = LAsc(Lam("x", Cursor(Var("x"))), Arrow(Num, Num));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
  let ht: Hazelnut.Htyp.t = Arrow(Num,Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      LAsc(Lam("x", RPlus(Var("x"),Cursor(EHole))), Arrow(Num, Num))
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let test_st12 = () => {
    let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
    let ze: Hazelnut.Zexp.t = LAsc(Lam("x", RPlus(Var("x"),Cursor(EHole))), Arrow(Num, Num));
    let t: Hazelnut.Htyp.t = Arrow(Num, Num);
    let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lit(1));
    let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
      Hazelnut.syn_action(ctx, (ze, t), a);
    let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
      Some((
        LAsc(Lam("x", RPlus(Var("x"),Cursor(Lit(1)))), Arrow(Num, Num)),
        Arrow(Num, Num),
      ));
    check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_ast12 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = LAsc(Lam("x", RPlus(Var("x"),Cursor(EHole))), Arrow(Num, Num));
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lit(1));
  let ht: Hazelnut.Htyp.t = Arrow(Num,Num);
  let given: option(Hazelnut.Zexp.t) = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected: option(Hazelnut.Zexp.t) =
    Some(
      LAsc(Lam("x", RPlus(Var("x"),Cursor(Lit(1)))), Arrow(Num, Num))
    );
  check(zexp_typ, "same Hazelnut.Zexp.t", given, expected);
};

let sample1_tests = [
  ("test_st1", `Quick, test_st1),
  ("test_ast1", `Quick, test_ast1),
  ("test_st2", `Quick, test_st2),
  ("test_ast2", `Quick, test_ast2),
  ("test_st3", `Quick, test_st3),
  ("test_ast3", `Quick, test_ast3),
  ("test_st4", `Quick, test_st4),
  ("test_ast4", `Quick, test_ast4),
  ("test_st5", `Quick, test_st5),
  ("test_ast5", `Quick, test_ast5),
  ("test_st6", `Quick, test_st6),
  ("test_ast6", `Quick, test_ast6),
  ("test_st7", `Quick, test_st7),
  ("test_ast7", `Quick, test_ast7),
  ("test_st8", `Quick, test_st8),
  ("test_ast8", `Quick, test_ast8),
  ("test_st9", `Quick, test_st9),
  ("test_ast9", `Quick, test_ast9),
  ("test_st10", `Quick, test_st10),
  ("test_ast10", `Quick, test_ast10),
  ("test_st11", `Quick, test_st11),
  ("test_ast11", `Quick, test_ast11),
  ("test_st12", `Quick, test_st12),
  ("test_ast12", `Quick, test_ast12),
];