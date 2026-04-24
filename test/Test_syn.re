open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

// Synthesis rules (Hazelnut 2017 Appendix A.1.3).
// Every compound rule (SAsc, SAp, SPlus, SNEHole) has at least one test
// whose sub-expressions are themselves compound — not just Lit/Var — so
// that a recursive synthesis bug at the second level is exposed.

// ==================================================================
// SAsc: Γ ⊢ ė ⇐ τ̇  =>  Γ ⊢ ė : τ̇ ⇒ τ̇
// ==================================================================

let test_sasc_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t =
    Asc(Lam("x", Plus(Lit(1), Lit(2))), Arrow(Num, Num));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) =
    Some(Hazelnut.Htyp.Arrow(Num, Num));
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// NEHole can be ana'd against Hole (via ASubsume: NEHole syn's Hole, Hole~Hole).
let test_sasc_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let he: Hazelnut.Hexp.t = Asc(NEHole(Var("x")), Hole);
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// SAsc whose body is itself an ascription of a non-value.
let test_sasc_3 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t =
    Asc(Asc(Plus(Lit(1), Lit(2)), Num), Num);
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// SAsc where inner expression fails to analyze at the annotated type
// (Plus analyzes at Num, but we claim Arrow(Num,Num), so this fails).
let test_sasc_4_fail = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t =
    Asc(Plus(Lit(1), Lit(2)), Arrow(Num, Num));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = None;
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// SAsc of a lambda whose body references the bound variable.
let test_sasc_5 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t =
    Asc(
      Lam("f", Lam("x", Ap(Var("f"), Var("x")))),
      Arrow(Arrow(Num, Num), Arrow(Num, Num)),
    );
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) =
    Some(Hazelnut.Htyp.Arrow(Arrow(Num, Num), Arrow(Num, Num)));
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// ==================================================================
// SVar: Γ, x : τ̇ ⊢ x ⇒ τ̇
// ==================================================================

let test_svar_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Var("x");
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = None;
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

let test_svar_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let he: Hazelnut.Hexp.t = Var("x");
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// SVar with arrow type.
let test_svar_3 = () => {
  let ctx: typctx =
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Arrow(Num, Num)));
  let he: Hazelnut.Hexp.t = Var("f");
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) =
    Some(Hazelnut.Htyp.Arrow(Num, Arrow(Num, Num)));
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// Lookup picks the right name when several are in scope.
let test_svar_4 = () => {
  let ctx: typctx =
    TypCtx.empty
    |> TypCtx.add("x", Hazelnut.Htyp.Num)
    |> TypCtx.add("f", Hazelnut.Htyp.Arrow(Num, Num))
    |> TypCtx.add("y", Hazelnut.Htyp.Hole);
  let he: Hazelnut.Hexp.t = Var("f");
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) =
    Some(Hazelnut.Htyp.Arrow(Num, Num));
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// ==================================================================
// SAp: Γ ⊢ ė₁ ⇒ τ̇, τ̇ ▸→ (τ̇₂ → τ̇′), Γ ⊢ ė₂ ⇐ τ̇₂  =>  Γ ⊢ ė₁(ė₂) ⇒ τ̇′
// MAHole: Hole ▸→ (Hole → Hole);  MAArr: (τ̇₁→τ̇₂) ▸→ (τ̇₁→τ̇₂)
// ==================================================================

let test_sap_1 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = Ap(Var("x"), Lit(1));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// Higher-order — argument is a lambda.
let test_sap_2 = () => {
  let ctx: typctx =
    TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Arrow(Num, Num), Num));
  let he: Hazelnut.Hexp.t = Ap(Var("x"), Lam("y", Lit(1)));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// SAp whose function position is itself a compound expression (an Ap).
// Forces correct recursive synthesis rather than matching only on Var/Lam.
let test_sap_3_curried = () => {
  let ctx: typctx =
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Arrow(Num, Num)));
  let he: Hazelnut.Hexp.t = Ap(Ap(Var("f"), Lit(1)), Lit(2));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// SAp where the argument is itself a compound Plus expression.
let test_sap_4_compound_arg = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = Ap(Var("f"), Plus(Lit(1), Plus(Lit(2), Lit(3))));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// MAHole: applying a hole synthesizes Hole (because Hole ▸→ Hole→Hole).
let test_sap_5_mahole = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Ap(EHole, Lit(1));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// MAHole via a NEHole in function position.
let test_sap_6_mahole_nehole = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Ap(NEHole(Lit(1)), Plus(Lit(1), Lit(2)));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// SAp fails when the function synthesizes Num (which doesn't match arrow).
let test_sap_7_fail_not_arrow = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Ap(Lit(5), Lit(1));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = None;
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// SAp fails when the argument does not analyze against the expected type:
// f : Num → Num, but applied to λy. y (which cannot ana at Num).
let test_sap_8_fail_bad_arg = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = Ap(Var("f"), Lam("y", Var("y")));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = None;
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// ==================================================================
// SNum: Γ ⊢ n ⇒ num
// ==================================================================

let test_snum_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Lit(1);
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

let test_snum_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Lit(-1);
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// ==================================================================
// SPlus: Γ ⊢ ė₁ ⇐ num, Γ ⊢ ė₂ ⇐ num  =>  Γ ⊢ ė₁ + ė₂ ⇒ num
// Educational note: common bug is to write SPlus so that it only recurses
// into value forms. Tests here deliberately put non-value forms on BOTH
// sides of a Plus.
// ==================================================================

let test_splus_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Plus(Lit(1), Lit(-1));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

let test_splus_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let he: Hazelnut.Hexp.t = Plus(Lit(1), Var("x"));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// "Plus of Plusses" — both children are themselves compound Plus expressions.
let test_splus_3_both_compound = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t =
    Plus(Plus(Lit(2), Lit(3)), Plus(Lit(4), Lit(5)));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// Plus over application results.
let test_splus_4_ap_sides = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t =
    Plus(Ap(Var("f"), Lit(1)), Ap(Var("f"), Lit(2)));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// SPlus: an Asc on one side that analyzes at Num.
let test_splus_5_asc_side = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t =
    Plus(Asc(Plus(Lit(1), Lit(2)), Num), Lit(3));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// SPlus with a Hole-typed variable on one side (subsumes because Hole~num).
let test_splus_6_hole_typed_var = () => {
  let ctx: typctx = TypCtx.singleton("h", Hazelnut.Htyp.Hole);
  let he: Hazelnut.Hexp.t = Plus(Var("h"), Lit(1));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// SPlus fails when a side cannot analyze at Num (Lam can't ana at Num).
let test_splus_7_fail_lam_side = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Plus(Lam("x", Lit(1)), Lit(2));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = None;
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// SPlus fails when a side synthesizes Arrow (not consistent with Num).
let test_splus_8_fail_arrow_side = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = Plus(Var("f"), Lit(1));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = None;
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// ==================================================================
// SEHole: Γ ⊢ ∅ ⇒ ⦇⦈
// ==================================================================

let test_shole_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = EHole;
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// Variable whose declared type is Hole also synthesizes Hole (via SVar).
let test_shole_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Hole);
  let he: Hazelnut.Hexp.t = Var("x");
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// ==================================================================
// SNEHole: Γ ⊢ ė ⇒ τ̇  =>  Γ ⊢ ⦇ė⦈ ⇒ ⦇⦈
// Note: the INNER must synthesize, but the outer ALWAYS synthesizes Hole.
// ==================================================================

let test_snehole_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = NEHole(EHole);
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// NEHole of an application result (not a value).
let test_snehole_2 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = NEHole(Asc(Ap(Var("incr"), Lit(1)), Num));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// Nested NEHole: ⦇⦇e⦈⦈ still syns Hole — provided the INNER e syns.
let test_snehole_3_nested = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t =
    NEHole(NEHole(Plus(Lit(1), Plus(Lit(2), Lit(3)))));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// A NEHole whose body fails to synthesize must itself fail: the SNEHole
// rule REQUIRES the inner expression to synthesize some type.
let test_snehole_4_fail_inner = () => {
  let ctx: typctx = TypCtx.empty; // no x in scope
  let he: Hazelnut.Hexp.t = NEHole(Var("x"));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = None;
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// A NEHole whose body is a Lam (which does NOT synthesize) must fail.
let test_snehole_5_fail_lam = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = NEHole(Lam("x", Lit(1)));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = None;
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

// ==================================================================
// Lam does NOT synthesize (only analyzes, via ALam).
// ==================================================================

let test_lam_no_syn = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Lam("x", Lit(1));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = None;
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

let syn_tests = [
  ("test_sasc_1", `Quick, test_sasc_1),
  ("test_sasc_2", `Quick, test_sasc_2),
  ("test_sasc_3", `Quick, test_sasc_3),
  ("test_sasc_4_fail", `Quick, test_sasc_4_fail),
  ("test_sasc_5", `Quick, test_sasc_5),
  ("test_svar_1", `Quick, test_svar_1),
  ("test_svar_2", `Quick, test_svar_2),
  ("test_svar_3", `Quick, test_svar_3),
  ("test_svar_4", `Quick, test_svar_4),
  ("test_sap_1", `Quick, test_sap_1),
  ("test_sap_2", `Quick, test_sap_2),
  ("test_sap_3_curried", `Quick, test_sap_3_curried),
  ("test_sap_4_compound_arg", `Quick, test_sap_4_compound_arg),
  ("test_sap_5_mahole", `Quick, test_sap_5_mahole),
  ("test_sap_6_mahole_nehole", `Quick, test_sap_6_mahole_nehole),
  ("test_sap_7_fail_not_arrow", `Quick, test_sap_7_fail_not_arrow),
  ("test_sap_8_fail_bad_arg", `Quick, test_sap_8_fail_bad_arg),
  ("test_snum_1", `Quick, test_snum_1),
  ("test_snum_2", `Quick, test_snum_2),
  ("test_splus_1", `Quick, test_splus_1),
  ("test_splus_2", `Quick, test_splus_2),
  ("test_splus_3_both_compound", `Quick, test_splus_3_both_compound),
  ("test_splus_4_ap_sides", `Quick, test_splus_4_ap_sides),
  ("test_splus_5_asc_side", `Quick, test_splus_5_asc_side),
  ("test_splus_6_hole_typed_var", `Quick, test_splus_6_hole_typed_var),
  ("test_splus_7_fail_lam_side", `Quick, test_splus_7_fail_lam_side),
  ("test_splus_8_fail_arrow_side", `Quick, test_splus_8_fail_arrow_side),
  ("test_shole_1", `Quick, test_shole_1),
  ("test_shole_2", `Quick, test_shole_2),
  ("test_snehole_1", `Quick, test_snehole_1),
  ("test_snehole_2", `Quick, test_snehole_2),
  ("test_snehole_3_nested", `Quick, test_snehole_3_nested),
  ("test_snehole_4_fail_inner", `Quick, test_snehole_4_fail_inner),
  ("test_snehole_5_fail_lam", `Quick, test_snehole_5_fail_lam),
  ("test_lam_no_syn", `Quick, test_lam_no_syn),
];
