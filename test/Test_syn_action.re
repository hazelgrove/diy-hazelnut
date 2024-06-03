open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

let test_sadel_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Del; 
  let given: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) =
    Some((Cursor(EHole), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_saconasc_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Asc); 
  let given: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) =
    Some((RAsc(Lit(1), Cursor(Num)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_saconvar_1 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("x")); 
  let given: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) =
    Some((Cursor(Var("x")), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_saconlam_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lam("x")); 
  let given: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) =
    Some((RAsc(Lam("x",EHole), LArrow(Cursor(Hole),Hole)), Arrow(Hole,Hole)));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_saconnumlit_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lit(1)); 
  let given: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) =
    Some((Cursor(Lit(1)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_saconnehole_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.NEHole); 
  let given: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) =
    Some((NEHole(Cursor(Lit(1))), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_saconaparr_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lam("x",EHole));
  let t: Hazelnut.Htyp.t = Arrow(Hole,Hole);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap); 
  let given: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) =
    Some((RAp(Lam("x",EHole), Cursor(EHole)), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_saconapotw_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap); 
  let given: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) =
    Some((RAp(NEHole(Lit(1)), Cursor(EHole)), Hole));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_saconplus1_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus); 
  let given: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) =
    Some((RPlus(Lit(1), Cursor(EHole)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_saconplus2_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lam("x",EHole));
  let t: Hazelnut.Htyp.t = Arrow(Hole,Hole);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus); 
  let given: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) =
    Some((RPlus(NEHole(Lam("x",EHole)), Cursor(EHole)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let test_safinish_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(NEHole(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Finish; 
  let given: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected: option((Hazelnut.Zexp.t,Hazelnut.Htyp.t)) =
    Some((Cursor(Lit(1)), Num));
  check(zexp_htyp, "same option(Hazelnut.Zexp.t)", given, expected);
};

let syn_action_tests = [
  ("test_sadel_1", `Quick, test_sadel_1),
  ("test_safinish_1", `Quick, test_safinish_1),
  ("test_saconasc_1", `Quick, test_saconasc_1),
  ("test_saconvar_1", `Quick, test_saconvar_1),
  ("test_saconlam_1", `Quick, test_saconlam_1),
  ("test_saconnumlit_1", `Quick, test_saconnumlit_1),
  ("test_saconnehole_1", `Quick, test_saconnehole_1),
  ("test_saconaparr_1", `Quick, test_saconaparr_1),
  ("test_saconapotw_1", `Quick, test_saconapotw_1),
  ("test_saconplus1_1", `Quick, test_saconplus1_1),
  ("test_saconplus2_1", `Quick, test_saconplus2_1),
];
