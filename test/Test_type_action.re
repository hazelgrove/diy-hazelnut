open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

let test_type_move1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), Cursor(Arrow(Hole, Hole)));
  let t: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let a: Hazelnut.Action.t = Move(Child(One));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(Lam("x", EHole), LArrow(Cursor(Hole), Hole)),
      Arrow(Hole, Hole),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_type_move2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), Cursor(Arrow(Hole, Hole)));
  let t: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let a: Hazelnut.Action.t = Move(Child(Two));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(Lam("x", EHole), RArrow(Hole, Cursor(Hole))),
      Arrow(Hole, Hole),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_type_move3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), RArrow(Hole, Cursor(Hole)));
  let t: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(Lam("x", EHole), Cursor(Arrow(Hole, Hole))),
      Arrow(Hole, Hole),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_type_move4 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", EHole), LArrow(Cursor(Hole), Hole));
  let t: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(Lam("x", EHole), Cursor(Arrow(Hole, Hole))),
      Arrow(Hole, Hole),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_type_del = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAsc(Lit(1), Cursor(Num));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Del;
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RAsc(Lit(1), Cursor(Hole)), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_type_conarr = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAsc(Lam("x", Lit(1)), Cursor(Num));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Arrow);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(Lam("x", Lit(1)), RArrow(Num, Cursor(Hole))),
      Arrow(Num, Hole),
    ));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};
let test_type_connum = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAsc(Lit(1), Cursor(Hole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Num);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((RAsc(Lit(1), Cursor(Num)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

// ==================================================================
// TMARRZIP1: τ̂ --α--> τ̂' => (τ̂ → τ̇) --α--> (τ̂' → τ̇)
// TMARRZIP2: τ̂ --α--> τ̂' => (τ̇ → τ̂) --α--> (τ̇ → τ̂')
// These rules recurse into the left or right of an arrow type.
// ==================================================================

// ==================================================================
// TMARRZIP1: τ̂ --α--> τ̂' => (τ̂ → τ̇) --α--> (τ̂' → τ̇)
// TMARRZIP2: τ̂ --α--> τ̂' => (τ̇ → τ̂) --α--> (τ̇ → τ̂')
// These rules recurse into the left or right of an arrow type.
// ==================================================================

// TMARRZIP1: Construct(Num) on Hole in left of a nested arrow.
let test_type_zip1_connum = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(EHole, LArrow(LArrow(Cursor(Hole), Num), Num));
  let t: Hazelnut.Htyp.t = Arrow(Arrow(Hole, Num), Num);
  let a: Hazelnut.Action.t = Construct(Num);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(EHole, LArrow(LArrow(Cursor(Num), Num), Num)),
      Arrow(Arrow(Num, Num), Num),
    ));
  check(zexp_htyp, "same", given, expected);
};

// TMARRZIP2: Construct(Arrow) on right of nested arrow.
let test_type_zip2_conarrow = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(EHole, RArrow(Num, RArrow(Num, Cursor(Hole))));
  let t: Hazelnut.Htyp.t = Arrow(Num, Arrow(Num, Hole));
  let a: Hazelnut.Action.t = Construct(Arrow);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(EHole, RArrow(Num, RArrow(Num, RArrow(Hole, Cursor(Hole))))),
      Arrow(Num, Arrow(Num, Arrow(Hole, Hole))),
    ));
  check(zexp_htyp, "same", given, expected);
};

// TMARRZIP1: Del inside left of arrow erases type to Hole.
let test_type_zip1_del = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(EHole, LArrow(Cursor(Num), Num));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Del;
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(EHole, LArrow(Cursor(Hole), Num)),
      Arrow(Hole, Num),
    ));
  check(zexp_htyp, "same", given, expected);
};

// TMARRZIP2: Del inside right of arrow.
let test_type_zip2_del = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(EHole, RArrow(Num, Cursor(Num)));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Del;
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(EHole, RArrow(Num, Cursor(Hole))),
      Arrow(Num, Hole),
    ));
  check(zexp_htyp, "same", given, expected);
};

// Movement through nested arrow type.
let test_type_move_nested_child = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(
      Lam("f", EHole),
      LArrow(Cursor(Arrow(Num, Num)), Num),
    );
  let t: Hazelnut.Htyp.t = Arrow(Arrow(Num, Num), Num);
  let a: Hazelnut.Action.t = Move(Child(One));
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(
        Lam("f", EHole),
        LArrow(LArrow(Cursor(Num), Num), Num),
      ),
      Arrow(Arrow(Num, Num), Num),
    ));
  check(zexp_htyp, "same", given, expected);
};

// Movement parent from inside nested arrow.
let test_type_move_nested_parent = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(
      Lam("f", EHole),
      LArrow(LArrow(Cursor(Num), Num), Num),
    );
  let t: Hazelnut.Htyp.t = Arrow(Arrow(Num, Num), Num);
  let a: Hazelnut.Action.t = Move(Parent);
  let given: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t, Hazelnut.Htyp.t)) =
    Some((
      RAsc(
        Lam("f", EHole),
        LArrow(Cursor(Arrow(Num, Num)), Num),
      ),
      Arrow(Arrow(Num, Num), Num),
    ));
  check(zexp_htyp, "same", given, expected);
};

let type_action_tests = [
  ("test_type_move1", `Quick, test_type_move1),
  ("test_type_move2", `Quick, test_type_move2),
  ("test_type_move3", `Quick, test_type_move3),
  ("test_type_move4", `Quick, test_type_move4),
  ("test_type_del", `Quick, test_type_del),
  ("test_type_conarr", `Quick, test_type_conarr),
  ("test_type_connum", `Quick, test_type_connum),
  ("test_type_zip1_connum", `Quick, test_type_zip1_connum),
  ("test_type_zip2_conarrow", `Quick, test_type_zip2_conarrow),
  ("test_type_zip1_del", `Quick, test_type_zip1_del),
  ("test_type_zip2_del", `Quick, test_type_zip2_del),
  ("test_type_move_nested_child", `Quick, test_type_move_nested_child),
  ("test_type_move_nested_parent", `Quick, test_type_move_nested_parent),
];
