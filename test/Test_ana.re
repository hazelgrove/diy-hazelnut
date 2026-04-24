open Alcotest;
module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

// Analysis rules (Hazelnut 2017 Appendix A.1.3).
//
// There are only two ana rules:
//   ASubsume: Γ ⊢ ė ⇒ τ̇' ∧ τ̇ ~ τ̇'  =>  Γ ⊢ ė ⇐ τ̇
//   ALam:     τ̇ ▸→ (τ̇₁→τ̇₂) ∧ Γ, x:τ̇₁ ⊢ ė ⇐ τ̇₂  =>  Γ ⊢ (λx.ė) ⇐ τ̇
//
// Note: Type consistency (~) is reflexive, Hole is consistent with anything,
// and arrow-on-arrow decomposes componentwise.

// ==================================================================
// ASubsume
// ==================================================================

// Num is NOT consistent with Arrow, so ASubsume fails.
let test_asubsume_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Lit(1);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = false;
  check(bool, "same bool", given, expected);
};

// Reflexive consistency: Num ~ Num.
let test_asubsume_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let he: Hazelnut.Hexp.t = Var("x");
  let ht: Hazelnut.Htyp.t = Num;
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

// Hole is consistent with anything.
let test_asubsume_3 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = Var("x");
  let ht: Hazelnut.Htyp.t = Hole;
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

// Hole on the synthesized side also gives consistency.
let test_asubsume_4_hole_syn = () => {
  let ctx: typctx = TypCtx.singleton("h", Hazelnut.Htyp.Hole);
  let he: Hazelnut.Hexp.t = Var("h");
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

// TCArr: arrow compatibility decomposes component-wise.
let test_asubsume_5_arrow_arrow = () => {
  let ctx: typctx =
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Hole, Num));
  let he: Hazelnut.Hexp.t = Var("f");
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

// Arrow-arrow INconsistent: (Num → Num) vs (Num → Arrow(Num,Num)).
let test_asubsume_6_arrow_inconsistent = () => {
  let ctx: typctx =
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = Var("f");
  let ht: Hazelnut.Htyp.t = Arrow(Num, Arrow(Num, Num));
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = false;
  check(bool, "same bool", given, expected);
};

// ASubsume over compound syn-ing expressions: Plus(Plus(...), Plus(...)) ⇐ Num.
let test_asubsume_7_compound_plus = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t =
    Plus(Plus(Lit(2), Lit(3)), Plus(Lit(4), Lit(5)));
  let ht: Hazelnut.Htyp.t = Num;
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

// NEHole syns Hole, Hole~τ for all τ, so NEHole ana's at any type.
let test_asubsume_8_nehole = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = NEHole(Plus(Lit(1), Lit(2)));
  let ht: Hazelnut.Htyp.t = Arrow(Arrow(Num, Num), Hole);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

// ASubsume where the expression doesn't synthesize — must fail.
// Lam has no synthesis form, so if the type doesn't match arrow, both
// branches (ASubsume and ALam) fail. Here Lam ⇐ Num.
let test_asubsume_9_lam_at_num_fails = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Lam("x", Lit(1));
  let ht: Hazelnut.Htyp.t = Num;
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = false;
  check(bool, "same bool", given, expected);
};

// ==================================================================
// ALam
// ==================================================================

// ALam with a compound body. τ matched to arrow, body analyzed at τ₂.
let test_alam_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Lam("x", Plus(Lit(1), Lit(2)));
  let ht: Hazelnut.Htyp.t = Arrow(Arrow(Num, Num), Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

// ALam with the body using a variable that WAS in the outer context.
let test_alam_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let he: Hazelnut.Hexp.t = Lam("f", Plus(Var("x"), Lit(2)));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

// ALam where the body relies on the outer x having an arrow type; since
// the lambda's own binding shadows, the body sees the *inner* x.
let test_alam_3 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = Lam("f", Plus(Var("x"), Lit(2)));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = false;
  check(bool, "same bool", given, expected);
};

// ALam with shadowing: inner x shadows outer x.
let test_alam_4_shadow = () => {
  let ctx: typctx =
    TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Num, Num));
  // λx. (x + 1)  — inner x is Num, outer was Arrow.
  let he: Hazelnut.Hexp.t = Lam("x", Plus(Var("x"), Lit(1)));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

// ALam against Hole: MAHole gives Hole→Hole, body analyzed at Hole.
let test_alam_5_at_hole = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Lam("x", Plus(Var("x"), Lit(1)));
  let ht: Hazelnut.Htyp.t = Hole;
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

// ALam with nested lambdas.
let test_alam_6_nested = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t =
    Lam("f", Lam("x", Ap(Var("f"), Plus(Var("x"), Lit(1)))));
  let ht: Hazelnut.Htyp.t = Arrow(Arrow(Num, Num), Arrow(Num, Num));
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

// ALam fails when the expected type is Num (MA fails).
let test_alam_7_fail_at_num = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Lam("x", Lit(1));
  let ht: Hazelnut.Htyp.t = Num;
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = false;
  check(bool, "same bool", given, expected);
};

// ALam fails when the BODY doesn't analyze against the codomain.
let test_alam_8_fail_bad_body = () => {
  let ctx: typctx = TypCtx.empty;
  // body is a Lam (not Num-compatible) but codomain is Num.
  let he: Hazelnut.Hexp.t = Lam("x", Lam("y", Var("y")));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = false;
  check(bool, "same bool", given, expected);
};

// Body of lambda uses the bound variable; tests that λ actually extends ctx.
let test_alam_9_uses_bound_var = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Lam("x", Var("x"));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

// λf. f applied as higher-order — body is Ap with compound arg.
let test_alam_10_higher_order = () => {
  let ctx: typctx = TypCtx.singleton("g", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t =
    Lam("f", Ap(Var("f"), Ap(Var("g"), Plus(Lit(1), Lit(2)))));
  let ht: Hazelnut.Htyp.t = Arrow(Arrow(Num, Num), Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

let ana_tests = [
  ("test_asubsume_1", `Quick, test_asubsume_1),
  ("test_asubsume_2", `Quick, test_asubsume_2),
  ("test_asubsume_3", `Quick, test_asubsume_3),
  ("test_asubsume_4_hole_syn", `Quick, test_asubsume_4_hole_syn),
  ("test_asubsume_5_arrow_arrow", `Quick, test_asubsume_5_arrow_arrow),
  (
    "test_asubsume_6_arrow_inconsistent",
    `Quick,
    test_asubsume_6_arrow_inconsistent,
  ),
  ("test_asubsume_7_compound_plus", `Quick, test_asubsume_7_compound_plus),
  ("test_asubsume_8_nehole", `Quick, test_asubsume_8_nehole),
  (
    "test_asubsume_9_lam_at_num_fails",
    `Quick,
    test_asubsume_9_lam_at_num_fails,
  ),
  ("test_alam_1", `Quick, test_alam_1),
  ("test_alam_2", `Quick, test_alam_2),
  ("test_alam_3", `Quick, test_alam_3),
  ("test_alam_4_shadow", `Quick, test_alam_4_shadow),
  ("test_alam_5_at_hole", `Quick, test_alam_5_at_hole),
  ("test_alam_6_nested", `Quick, test_alam_6_nested),
  ("test_alam_7_fail_at_num", `Quick, test_alam_7_fail_at_num),
  ("test_alam_8_fail_bad_body", `Quick, test_alam_8_fail_bad_body),
  ("test_alam_9_uses_bound_var", `Quick, test_alam_9_uses_bound_var),
  ("test_alam_10_higher_order", `Quick, test_alam_10_higher_order),
];
