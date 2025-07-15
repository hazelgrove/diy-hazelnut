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

let type_action_tests = [
  ("test_type_move1", `Quick, test_type_move1),
  ("test_type_move2", `Quick, test_type_move2),
  ("test_type_move3", `Quick, test_type_move3),
  ("test_type_move4", `Quick, test_type_move4),
  ("test_type_del", `Quick, test_type_del),
  ("test_type_conarr", `Quick, test_type_conarr),
  ("test_type_connum", `Quick, test_type_connum),
];
