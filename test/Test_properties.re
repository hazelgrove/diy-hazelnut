open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

// Metatheorem verification tests (Hazelnut 2017, Theorems 1–2).
//
// These tests don't just check individual rule outputs — they verify
// the PROPERTIES that the metatheory guarantees:
//
//   Theorem 1 (Sensibility):
//     If Γ ⊢ ê ⇒ τ and Γ ⊢ ê ⇒ τ --α--> ê' ⇒ τ', then Γ ⊢ ê' ⇒ τ'.
//     (After a synthetic action, the erased result synthesizes the output type.)
//
//   Theorem 2 (Movement Erasure Invariance):
//     If Γ ⊢ ê ⇒ τ --move δ--> ê' ⇒ τ', then erase(ê) = erase(ê') and τ = τ'.
//     (Movement never changes the cursor erasure or synthesized type.)
//
// A correct implementation must satisfy these for ALL valid inputs, not just
// the ones that test individual rules. These tests exercise the properties on
// compound expressions that exercise multiple rules in combination.

let htyp_eq = (a: Hazelnut.Htyp.t, b: Hazelnut.Htyp.t) =>
  Hazelnut.Htyp.compare(a, b) == 0;

// Helper: assert that the sensibility theorem holds for a syn_action result.
// Given (ctx, ze, t, action), if syn_action produces Some((ze', t')), then:
//   erase(ze') must synthesize t' under ctx.
let check_sensibility =
    (
      label: string,
      ctx: typctx,
      ze: Hazelnut.Zexp.t,
      t: Hazelnut.Htyp.t,
      a: Hazelnut.Action.t,
    ) => {
  switch (Hazelnut.syn_action(ctx, (ze, t), a)) {
  | Some((ze', t')) =>
    let erased = Hazelnut.erase_exp(ze');
    let syn_result = Hazelnut.syn(ctx, erased);
    switch (syn_result) {
    | Some(t_syn) =>
      check(
        bool,
        label ++ ": erased result synthesizes output type",
        htyp_eq(t_syn, t'),
        true,
      )
    | None =>
      fail(label ++ ": erased result fails to synthesize any type")
    };
  | None => fail(label ++ ": action unexpectedly failed (returned None)")
  };
};

// Helper: assert movement erasure invariance.
let check_move_invariance =
    (
      label: string,
      ctx: typctx,
      ze: Hazelnut.Zexp.t,
      t: Hazelnut.Htyp.t,
      dir: Hazelnut.Dir.t,
    ) => {
  let erased_before = Hazelnut.erase_exp(ze);
  switch (Hazelnut.syn_action(ctx, (ze, t), Move(dir))) {
  | Some((ze', t')) =>
    let erased_after = Hazelnut.erase_exp(ze');
    check(
      hexp_typ,
      label ++ ": erasure unchanged",
      erased_before,
      erased_after,
    );
    check(
      bool,
      label ++ ": type unchanged",
      htyp_eq(t, t'),
      true,
    );
  | None => fail(label ++ ": move unexpectedly failed")
  };
};

// ==================================================================
// Sensibility: construct actions on compound expressions
// ==================================================================

let test_sensibility_convar = () =>
  check_sensibility(
    "convar",
    TypCtx.singleton("x", Hazelnut.Htyp.Num),
    Cursor(EHole),
    Hole,
    Construct(Var("x")),
  );

let test_sensibility_conlit = () =>
  check_sensibility(
    "conlit",
    TypCtx.empty,
    Cursor(EHole),
    Hole,
    Construct(Lit(42)),
  );

let test_sensibility_conlam = () =>
  check_sensibility(
    "conlam",
    TypCtx.empty,
    Cursor(EHole),
    Hole,
    Construct(Lam("x")),
  );

let test_sensibility_conap_arrow = () =>
  check_sensibility(
    "conap_arrow",
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num)),
    Cursor(Var("f")),
    Arrow(Num, Num),
    Construct(Ap),
  );

let test_sensibility_conap_non_arrow = () =>
  check_sensibility(
    "conap_non_arrow",
    TypCtx.empty,
    Cursor(Lit(1)),
    Num,
    Construct(Ap),
  );

let test_sensibility_conplus_consistent = () =>
  check_sensibility(
    "conplus_consistent",
    TypCtx.empty,
    Cursor(Lit(1)),
    Num,
    Construct(Plus),
  );

let test_sensibility_conplus_inconsistent = () =>
  check_sensibility(
    "conplus_inconsistent",
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num)),
    Cursor(Var("f")),
    Arrow(Num, Num),
    Construct(Plus),
  );

let test_sensibility_connehole = () =>
  check_sensibility(
    "connehole",
    TypCtx.empty,
    Cursor(Plus(Lit(1), Plus(Lit(2), Lit(3)))),
    Num,
    Construct(NEHole),
  );

let test_sensibility_conasc = () =>
  check_sensibility(
    "conasc",
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num)),
    Cursor(Ap(Var("f"), Lit(1))),
    Num,
    Construct(Asc),
  );

let test_sensibility_del = () =>
  check_sensibility(
    "del",
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num)),
    Cursor(Ap(Var("f"), Plus(Lit(1), Lit(2)))),
    Num,
    Del,
  );

let test_sensibility_finish = () =>
  check_sensibility(
    "finish",
    TypCtx.empty,
    Cursor(NEHole(Plus(Lit(1), Plus(Lit(2), Lit(3))))),
    Hole,
    Finish,
  );

// Sensibility through zipper rules: action deep inside compound expression.
let test_sensibility_zipper_plus = () =>
  check_sensibility(
    "zipper_plus",
    TypCtx.empty,
    LPlus(Cursor(Lit(1)), Plus(Lit(2), Lit(3))),
    Num,
    Construct(Plus),
  );

let test_sensibility_zipper_ap = () =>
  check_sensibility(
    "zipper_ap",
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num)),
    RAp(Var("f"), Cursor(EHole)),
    Num,
    Construct(Lit(5)),
  );

let test_sensibility_zipper_nehole = () =>
  check_sensibility(
    "zipper_nehole",
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num)),
    NEHole(Cursor(Var("f"))),
    Hole,
    Construct(Ap),
  );

let test_sensibility_zipper_asc_type = () =>
  check_sensibility(
    "zipper_asc_type",
    TypCtx.empty,
    RAsc(Lam("x", Lit(1)), Cursor(Num)),
    Num,
    Construct(Arrow),
  );

let test_sensibility_zipper_asc_expr = () =>
  check_sensibility(
    "zipper_asc_expr",
    TypCtx.empty,
    LAsc(Lam("x", Cursor(EHole)), Arrow(Num, Num)),
    Arrow(Num, Num),
    Construct(Var("x")),
  );

// ==================================================================
// Movement Erasure Invariance
// ==================================================================

let test_mei_plus_child1 = () =>
  check_move_invariance(
    "plus_child1",
    TypCtx.empty,
    Cursor(Plus(Plus(Lit(1), Lit(2)), Plus(Lit(3), Lit(4)))),
    Num,
    Child(One),
  );

let test_mei_plus_child2 = () =>
  check_move_invariance(
    "plus_child2",
    TypCtx.empty,
    Cursor(Plus(Plus(Lit(1), Lit(2)), Plus(Lit(3), Lit(4)))),
    Num,
    Child(Two),
  );

let test_mei_plus_parent1 = () =>
  check_move_invariance(
    "plus_parent1",
    TypCtx.empty,
    LPlus(Cursor(Plus(Lit(1), Lit(2))), Plus(Lit(3), Lit(4))),
    Num,
    Parent,
  );

let test_mei_plus_parent2 = () =>
  check_move_invariance(
    "plus_parent2",
    TypCtx.empty,
    RPlus(Plus(Lit(1), Lit(2)), Cursor(Plus(Lit(3), Lit(4)))),
    Num,
    Parent,
  );

let test_mei_ap_child1 = () =>
  check_move_invariance(
    "ap_child1",
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num)),
    Cursor(Ap(Var("f"), Plus(Lit(1), Lit(2)))),
    Num,
    Child(One),
  );

let test_mei_ap_child2 = () =>
  check_move_invariance(
    "ap_child2",
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num)),
    Cursor(Ap(Var("f"), Plus(Lit(1), Lit(2)))),
    Num,
    Child(Two),
  );

let test_mei_ap_parent_fn = () =>
  check_move_invariance(
    "ap_parent_fn",
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num)),
    LAp(Cursor(Var("f")), Plus(Lit(1), Lit(2))),
    Num,
    Parent,
  );

let test_mei_ap_parent_arg = () =>
  check_move_invariance(
    "ap_parent_arg",
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num)),
    RAp(Var("f"), Cursor(Plus(Lit(1), Lit(2)))),
    Num,
    Parent,
  );

let test_mei_asc_child1 = () =>
  check_move_invariance(
    "asc_child1",
    TypCtx.empty,
    Cursor(Asc(Lam("x", Plus(Var("x"), Lit(1))), Arrow(Num, Num))),
    Arrow(Num, Num),
    Child(One),
  );

let test_mei_asc_child2 = () =>
  check_move_invariance(
    "asc_child2",
    TypCtx.empty,
    Cursor(Asc(Lam("x", Plus(Var("x"), Lit(1))), Arrow(Num, Num))),
    Arrow(Num, Num),
    Child(Two),
  );

let test_mei_nehole_child1 = () =>
  check_move_invariance(
    "nehole_child1",
    TypCtx.empty,
    Cursor(NEHole(Plus(Lit(1), Lit(2)))),
    Hole,
    Child(One),
  );

let test_mei_nehole_parent = () =>
  check_move_invariance(
    "nehole_parent",
    TypCtx.empty,
    NEHole(Cursor(Plus(Lit(1), Lit(2)))),
    Hole,
    Parent,
  );

let test_mei_lam_child1 = () =>
  check_move_invariance(
    "lam_child1",
    TypCtx.empty,
    LAsc(Cursor(Lam("x", Plus(Lit(1), Lit(2)))), Arrow(Num, Num)),
    Arrow(Num, Num),
    Child(One),
  );

let test_mei_lam_parent = () =>
  check_move_invariance(
    "lam_parent",
    TypCtx.empty,
    LAsc(Lam("x", Cursor(Plus(Lit(1), Lit(2)))), Arrow(Num, Num)),
    Arrow(Num, Num),
    Parent,
  );

// Deep: movement through two levels of zipper.
let test_mei_deep_asc_arrow = () =>
  check_move_invariance(
    "deep_asc_arrow",
    TypCtx.empty,
    RAsc(Lam("x", EHole), Cursor(Arrow(Num, Num))),
    Arrow(Num, Num),
    Child(One),
  );

let properties_tests = [
  // Sensibility
  ("sensibility_convar", `Quick, test_sensibility_convar),
  ("sensibility_conlit", `Quick, test_sensibility_conlit),
  ("sensibility_conlam", `Quick, test_sensibility_conlam),
  ("sensibility_conap_arrow", `Quick, test_sensibility_conap_arrow),
  ("sensibility_conap_non_arrow", `Quick, test_sensibility_conap_non_arrow),
  (
    "sensibility_conplus_consistent",
    `Quick,
    test_sensibility_conplus_consistent,
  ),
  (
    "sensibility_conplus_inconsistent",
    `Quick,
    test_sensibility_conplus_inconsistent,
  ),
  ("sensibility_connehole", `Quick, test_sensibility_connehole),
  ("sensibility_conasc", `Quick, test_sensibility_conasc),
  ("sensibility_del", `Quick, test_sensibility_del),
  ("sensibility_finish", `Quick, test_sensibility_finish),
  ("sensibility_zipper_plus", `Quick, test_sensibility_zipper_plus),
  ("sensibility_zipper_ap", `Quick, test_sensibility_zipper_ap),
  ("sensibility_zipper_nehole", `Quick, test_sensibility_zipper_nehole),
  ("sensibility_zipper_asc_type", `Quick, test_sensibility_zipper_asc_type),
  ("sensibility_zipper_asc_expr", `Quick, test_sensibility_zipper_asc_expr),
  // Movement Erasure Invariance
  ("mei_plus_child1", `Quick, test_mei_plus_child1),
  ("mei_plus_child2", `Quick, test_mei_plus_child2),
  ("mei_plus_parent1", `Quick, test_mei_plus_parent1),
  ("mei_plus_parent2", `Quick, test_mei_plus_parent2),
  ("mei_ap_child1", `Quick, test_mei_ap_child1),
  ("mei_ap_child2", `Quick, test_mei_ap_child2),
  ("mei_ap_parent_fn", `Quick, test_mei_ap_parent_fn),
  ("mei_ap_parent_arg", `Quick, test_mei_ap_parent_arg),
  ("mei_asc_child1", `Quick, test_mei_asc_child1),
  ("mei_asc_child2", `Quick, test_mei_asc_child2),
  ("mei_nehole_child1", `Quick, test_mei_nehole_child1),
  ("mei_nehole_parent", `Quick, test_mei_nehole_parent),
  ("mei_lam_child1", `Quick, test_mei_lam_child1),
  ("mei_lam_parent", `Quick, test_mei_lam_parent),
  ("mei_deep_asc_arrow", `Quick, test_mei_deep_asc_arrow),
];
