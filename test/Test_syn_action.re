open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;
module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

// Synthetic action rules (Hazelnut 2017 Appendix A.3.3).
//
// Each rule name from the paper (SAMOVE, SADEL, SACONASC, SACONVAR,
// SACONLAM, SACONAPARR, SACONAPOTW, SACONNUMLIT, SACONPLUS1, SACONPLUS2,
// SACONNEHOLE, SAFINISH, SAZIPASC1, SAZIPASC2, SAZIPAPARR, SAZIPAPANA,
// SAZIPPLUS1, SAZIPPLUS2, SAZIPHOLE) has at least one test here.
//
// The zipper-rule tests are kept relatively small; more elaborate zipper
// tests live in Test_zipper.re.

// ==================================================================
// SAMove — delegates to the movement judgement ê --move δ--> ê'.
// Exhaustively covers the movement rules from A.3.2.
// ==================================================================

// EMAscChild1 : ▶e:τ◀ --child1--> ▶e◀ : τ
let test_samove_asc1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Asc(Lit(1), Num));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Child(One));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = Some((Hazelnut.Zexp.LAsc(Cursor(Lit(1)), Num), t));
  check(zexp_htyp, "same", given, expected);
};

// EMAscChild2 : ▶e:τ◀ --child2--> e : ▶τ◀
let test_samove_asc2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Asc(Lit(1), Num));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Child(Two));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = Some((Hazelnut.Zexp.RAsc(Lit(1), Cursor(Num)), t));
  check(zexp_htyp, "same", given, expected);
};

// EMAscParent1
let test_samove_asc3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = LAsc(Cursor(Lit(1)), Num);
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Parent);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = Some((Hazelnut.Zexp.Cursor(Asc(Lit(1), Num)), t));
  check(zexp_htyp, "same", given, expected);
};

// EMAscParent2
let test_samove_asc4 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAsc(Lit(1), Cursor(Num));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Parent);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = Some((Hazelnut.Zexp.Cursor(Asc(Lit(1), Num)), t));
  check(zexp_htyp, "same", given, expected);
};

// EMLamChild1: subject is compound to guard against leaf-only bugs.
let test_samove_lam1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    LAsc(Cursor(Lam("x", Plus(Lit(1), Lit(2)))), Arrow(Num, Num));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Move(Child(One));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  // Cursor moves to the subject of the Asc first, via SAMove of the Asc;
  // then we test Lam-child: but SAMove is applied via EMAscChild1 here.
  let expected =
    Some((
      Hazelnut.Zexp.LAsc(
        Lam("x", Cursor(Plus(Lit(1), Lit(2)))),
        Arrow(Num, Num),
      ),
      t,
    ));
  // Instead: we already are in LAsc(Cursor(Lam(...)), ...), so Child(1) of
  // the Lam moves to its body. That is what we expect.
  check(zexp_htyp, "same", given, expected);
};

// EMLamParent
let test_samove_lam2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    LAsc(Lam("x", Cursor(EHole)), Arrow(Hole, Hole));
  let t: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let a: Hazelnut.Action.t = Move(Parent);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LAsc(Cursor(Lam("x", EHole)), Arrow(Hole, Hole)),
      t,
    ));
  check(zexp_htyp, "same", given, expected);
};

// EMPlusChild1 — both siblings compound.
let test_samove_plus1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    Cursor(Plus(Plus(Lit(1), Lit(2)), Plus(Lit(3), Lit(4))));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Child(One));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LPlus(
        Cursor(Plus(Lit(1), Lit(2))),
        Plus(Lit(3), Lit(4)),
      ),
      t,
    ));
  check(zexp_htyp, "same", given, expected);
};

// EMPlusChild2
let test_samove_plus2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    Cursor(Plus(Plus(Lit(1), Lit(2)), Plus(Lit(3), Lit(4))));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Child(Two));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RPlus(
        Plus(Lit(1), Lit(2)),
        Cursor(Plus(Lit(3), Lit(4))),
      ),
      t,
    ));
  check(zexp_htyp, "same", given, expected);
};

// EMPlusParent1
let test_samove_plus3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = LPlus(Cursor(Lit(1)), Lit(2));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Parent);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = Some((Hazelnut.Zexp.Cursor(Plus(Lit(1), Lit(2))), t));
  check(zexp_htyp, "same", given, expected);
};

// EMPlusParent2
let test_samove_plus4 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RPlus(Lit(1), Cursor(Lit(2)));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Parent);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = Some((Hazelnut.Zexp.Cursor(Plus(Lit(1), Lit(2))), t));
  check(zexp_htyp, "same", given, expected);
};

// EMApChild1 — both operands compound.
let test_samove_ap1 = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    Cursor(Ap(Ap(Var("f"), Lit(1)), Plus(Lit(2), Lit(3))));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Move(Child(One));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LAp(
        Cursor(Ap(Var("f"), Lit(1))),
        Plus(Lit(2), Lit(3)),
      ),
      t,
    ));
  check(zexp_htyp, "same", given, expected);
};

// EMApChild2
let test_samove_ap2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Ap(Lam("x", EHole), EHole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Move(Child(Two));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.RAp(Lam("x", EHole), Cursor(EHole)), t));
  check(zexp_htyp, "same", given, expected);
};

// EMApParent1
let test_samove_ap3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = LAp(Cursor(Lam("x", EHole)), EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Move(Parent);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.Cursor(Ap(Lam("x", EHole), EHole)), t));
  check(zexp_htyp, "same", given, expected);
};

// EMApParent2
let test_samove_ap4 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAp(Lam("x", EHole), Cursor(EHole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Move(Parent);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.Cursor(Ap(Lam("x", EHole), EHole)), t));
  check(zexp_htyp, "same", given, expected);
};

// EMNEHoleChild1
let test_samove_neh1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(NEHole(Plus(Lit(1), Lit(2))));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Move(Child(One));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.NEHole(Cursor(Plus(Lit(1), Lit(2)))), t));
  check(zexp_htyp, "same", given, expected);
};

// EMNEHoleParent
let test_samove_neh2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Move(Parent);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = Some((Hazelnut.Zexp.Cursor(NEHole(Lit(1))), t));
  check(zexp_htyp, "same", given, expected);
};

// Moving past the top of the tree is not a valid action.
let test_samove_top_parent_none = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Parent);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = None;
  check(zexp_htyp, "same", given, expected);
};

// Moving into a leaf (like Var) is not a valid action.
let test_samove_leaf_child_none = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = Cursor(Var("x"));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Move(Child(One));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = None;
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SADel: ▶ė◀ --del--> ▶∅◀ : ⦇⦈
// ==================================================================

let test_sadel_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Del;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = Some((Hazelnut.Zexp.Cursor(EHole), Hazelnut.Htyp.Hole));
  check(zexp_htyp, "same", given, expected);
};

// Del applied through an NEHole zipper — checks SAZipHole routing too.
let test_sadel_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Del;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.NEHole(Cursor(EHole)), Hazelnut.Htyp.Hole));
  check(zexp_htyp, "same", given, expected);
};

// Del of a COMPOUND expression (not a value form).
let test_sadel_3_compound = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    Cursor(Ap(Var("f"), Plus(Lit(1), Plus(Lit(2), Lit(3)))));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Del;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = Some((Hazelnut.Zexp.Cursor(EHole), Hazelnut.Htyp.Hole));
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAConAsc: ▶ė◀ --construct asc--> ė : ▶τ̇◀
// The initial ascription type is the previously-synthesized τ.
// ==================================================================

let test_saconasc_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Asc);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.RAsc(Lit(1), Cursor(Num)), Hazelnut.Htyp.Num));
  check(zexp_htyp, "same", given, expected);
};

let test_saconasc_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Asc);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.RAsc(EHole, Cursor(Hole)), Hazelnut.Htyp.Hole));
  check(zexp_htyp, "same", given, expected);
};

// SAConAsc on a compound expression (applied via SAZipHole).
let test_saconasc_3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Asc);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.NEHole(RAsc(Lit(1), Cursor(Num))),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAConAsc on a compound subject (Plus), at the top level.
let test_saconasc_4_compound = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Plus(Lit(1), Plus(Lit(2), Lit(3))));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Asc);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAsc(Plus(Lit(1), Plus(Lit(2), Lit(3))), Cursor(Num)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAConVar: Γ, x:τ ⊢ ▶∅◀ --construct var x--> ▶x◀ : τ
// Only applies when the cursor is at EHole.
// ==================================================================

let test_saconvar_1 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("x"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = Some((Hazelnut.Zexp.Cursor(Var("x")), Hazelnut.Htyp.Num));
  check(zexp_htyp, "same", given, expected);
};

// SAConVar inside an NEHole (routes through SAZipHole).
let test_saconvar_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(EHole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("x"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.NEHole(Cursor(Var("x"))),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAConVar with arrow type in context.
let test_saconvar_3_arrow = () => {
  let ctx: typctx =
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Arrow(Num, Num)));
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("f"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.Cursor(Var("f")),
      Hazelnut.Htyp.Arrow(Num, Arrow(Num, Num)),
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAConVar: if the variable is not in scope, the action should fail.
let test_saconvar_4_not_in_scope = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("z"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = None;
  check(zexp_htyp, "same", given, expected);
};

// SAConVar applied at a non-hole cursor: undefined in the paper and so
// the action should fail.
let test_saconvar_5_non_hole_fails = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = Cursor(Lit(5));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Var("x"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = None;
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAConLam: Γ ⊢ ▶∅◀ --construct lam x--> (λx.∅) : (▶⦇⦈◀ → ⦇⦈) : Hole→Hole
// ==================================================================

let test_saconlam_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lam("x"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAsc(Lam("x", EHole), LArrow(Cursor(Hole), Hole)),
      Hazelnut.Htyp.Arrow(Hole, Hole),
    ));
  check(zexp_htyp, "same", given, expected);
};

let test_saconlam_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(EHole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lam("x"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.NEHole(
        RAsc(Lam("x", EHole), LArrow(Cursor(Hole), Hole)),
      ),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAConLam only applies at EHole; must fail on non-hole under syn.
let test_saconlam_3_non_hole_fails = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(5));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lam("y"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = None;
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAConNumLit: ▶∅◀ --construct lit n--> ▶n◀ : num
// ==================================================================

let test_saconnumlit_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lit(1));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = Some((Hazelnut.Zexp.Cursor(Lit(1)), Hazelnut.Htyp.Num));
  check(zexp_htyp, "same", given, expected);
};

let test_saconnumlit_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(EHole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lit(42));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.NEHole(Cursor(Lit(42))), Hazelnut.Htyp.Hole));
  check(zexp_htyp, "same", given, expected);
};

// Construct lit must fail at a non-hole cursor (rule only defined on ∅).
let test_saconnumlit_3_non_hole_fails = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(3));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Lit(4));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = None;
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAConNEHole: ▶ė◀ --construct nehole--> ⦇▶ė◀⦈ : Hole
// ==================================================================

let test_saconnehole_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.NEHole);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.NEHole(Cursor(Lit(1))), Hazelnut.Htyp.Hole));
  check(zexp_htyp, "same", given, expected);
};

let test_saconnehole_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.NEHole);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.NEHole(NEHole(Cursor(Lit(1)))),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Wrap a compound expression (not a leaf) in NEHole.
let test_saconnehole_3_compound = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    Cursor(Ap(Var("f"), Plus(Lit(1), Lit(2))));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.NEHole);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.NEHole(Cursor(Ap(Var("f"), Plus(Lit(1), Lit(2))))),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAConApArr: τ̇ ▸→ (τ̇₁→τ̇₂)  =>  ▶ė◀ --construct ap--> ė(▶∅◀) : τ̇₂
// SAConApOtw: τ̇ ⌿▸→        =>  ▶ė◀ --construct ap--> ⦇ė⦈(▶∅◀) : Hole
// ==================================================================

// SAConApArr: τ is Arrow.
let test_saconaparr_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lam("x", EHole));
  let t: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAp(Lam("x", EHole), Cursor(EHole)),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAConApArr: τ is Arrow(Num, Num) → result type is Num (the codomain).
let test_saconaparr_2_codomain = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(Var("f"));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAp(Var("f"), Cursor(EHole)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// MAHole path: τ is Hole → result type is Hole.
let test_saconaparr_3_mahole = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.RAp(EHole, Cursor(EHole)), Hazelnut.Htyp.Hole));
  check(zexp_htyp, "same", given, expected);
};

// SAConApOtw: τ is Num (doesn't match arrow) → wraps function in NEHole.
let test_saconapotw_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAp(NEHole(Lit(1)), Cursor(EHole)),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

let test_saconapotw_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.NEHole(RAp(NEHole(Lit(1)), Cursor(EHole))),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAConApOtw on a compound Plus (syns Num, doesn't match arrow).
let test_saconapotw_3_compound = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Plus(Lit(1), Plus(Lit(2), Lit(3))));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Ap);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAp(
        NEHole(Plus(Lit(1), Plus(Lit(2), Lit(3)))),
        Cursor(EHole),
      ),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAConPlus1: τ̇ ~ num  =>  ▶ė◀ --construct plus--> (ė + ▶∅◀) : num
// SAConPlus2: τ̇ ⌿~ num  =>  ▶ė◀ --construct plus--> (⦇ė⦈ + ▶∅◀) : num
// ==================================================================

// SAConPlus1: τ = Num (consistent).
let test_saconplus1_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RPlus(Lit(1), Cursor(EHole)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

let test_saconplus1_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.NEHole(RPlus(Lit(1), Cursor(EHole))),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAConPlus1 on Hole: Hole ~ Num is true.
let test_saconplus1_3_hole = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RPlus(EHole, Cursor(EHole)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAConPlus1 on a compound expression that syns Num.
let test_saconplus1_4_compound = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(Ap(Var("f"), Lit(1)));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RPlus(Ap(Var("f"), Lit(1)), Cursor(EHole)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAConPlus2: τ = Arrow(Hole, Hole) (not consistent with Num).
let test_saconplus2_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lam("x", EHole));
  let t: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RPlus(NEHole(Lam("x", EHole)), Cursor(EHole)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAConPlus2: τ = Arrow(Num, Num) (not consistent with Num).
let test_saconplus2_2 = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(Var("f"));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Construct(Hazelnut.Shape.Plus);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RPlus(NEHole(Var("f")), Cursor(EHole)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAFinish: ▶⦇ė⦈◀ --finish--> ▶ė◀ (when ė synthesizes some τ̇')
// ==================================================================

let test_safinish_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(NEHole(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Finish;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = Some((Hazelnut.Zexp.Cursor(Lit(1)), Hazelnut.Htyp.Num));
  check(zexp_htyp, "same", given, expected);
};

// Finish inside an NEHole (routes through SAZipHole, then SAFinish).
let test_safinish_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(NEHole(Lit(1))));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Finish;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.NEHole(Cursor(Lit(1))), Hazelnut.Htyp.Hole));
  check(zexp_htyp, "same", given, expected);
};

// SAFinish on a NEHole containing a compound expression that synthesizes.
let test_safinish_3_compound = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    Cursor(NEHole(Plus(Lit(1), Plus(Lit(2), Lit(3)))));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Finish;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.Cursor(Plus(Lit(1), Plus(Lit(2), Lit(3)))),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAFinish recovers an arrow type when the NEHole contains an ascribed lambda.
let test_safinish_4_arrow = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    Cursor(NEHole(Asc(Lam("x", Var("x")), Arrow(Num, Num))));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Finish;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.Cursor(Asc(Lam("x", Var("x")), Arrow(Num, Num))),
      Hazelnut.Htyp.Arrow(Num, Num),
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAFinish fails if the body of the NEHole does not synthesize.
// (Lam alone doesn't synthesize.)
let test_safinish_5_fail_no_syn = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(NEHole(Lam("x", Lit(1))));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Finish;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = None;
  check(zexp_htyp, "same", given, expected);
};

let syn_action_tests = [
  // Move
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
  ("test_samove_top_parent_none", `Quick, test_samove_top_parent_none),
  ("test_samove_leaf_child_none", `Quick, test_samove_leaf_child_none),
  // Del
  ("test_sadel_1", `Quick, test_sadel_1),
  ("test_sadel_2", `Quick, test_sadel_2),
  ("test_sadel_3_compound", `Quick, test_sadel_3_compound),
  // Finish
  ("test_safinish_1", `Quick, test_safinish_1),
  ("test_safinish_2", `Quick, test_safinish_2),
  ("test_safinish_3_compound", `Quick, test_safinish_3_compound),
  ("test_safinish_4_arrow", `Quick, test_safinish_4_arrow),
  ("test_safinish_5_fail_no_syn", `Quick, test_safinish_5_fail_no_syn),
  // Construct Asc
  ("test_saconasc_1", `Quick, test_saconasc_1),
  ("test_saconasc_2", `Quick, test_saconasc_2),
  ("test_saconasc_3", `Quick, test_saconasc_3),
  ("test_saconasc_4_compound", `Quick, test_saconasc_4_compound),
  // Construct Var
  ("test_saconvar_1", `Quick, test_saconvar_1),
  ("test_saconvar_2", `Quick, test_saconvar_2),
  ("test_saconvar_3_arrow", `Quick, test_saconvar_3_arrow),
  ("test_saconvar_4_not_in_scope", `Quick, test_saconvar_4_not_in_scope),
  ("test_saconvar_5_non_hole_fails", `Quick, test_saconvar_5_non_hole_fails),
  // Construct Lam
  ("test_saconlam_1", `Quick, test_saconlam_1),
  ("test_saconlam_2", `Quick, test_saconlam_2),
  ("test_saconlam_3_non_hole_fails", `Quick, test_saconlam_3_non_hole_fails),
  // Construct NumLit
  ("test_saconnumlit_1", `Quick, test_saconnumlit_1),
  ("test_saconnumlit_2", `Quick, test_saconnumlit_2),
  (
    "test_saconnumlit_3_non_hole_fails",
    `Quick,
    test_saconnumlit_3_non_hole_fails,
  ),
  // Construct NEHole
  ("test_saconnehole_1", `Quick, test_saconnehole_1),
  ("test_saconnehole_2", `Quick, test_saconnehole_2),
  ("test_saconnehole_3_compound", `Quick, test_saconnehole_3_compound),
  // Construct Ap
  ("test_saconaparr_1", `Quick, test_saconaparr_1),
  ("test_saconaparr_2_codomain", `Quick, test_saconaparr_2_codomain),
  ("test_saconaparr_3_mahole", `Quick, test_saconaparr_3_mahole),
  ("test_saconapotw_1", `Quick, test_saconapotw_1),
  ("test_saconapotw_2", `Quick, test_saconapotw_2),
  ("test_saconapotw_3_compound", `Quick, test_saconapotw_3_compound),
  // Construct Plus
  ("test_saconplus1_1", `Quick, test_saconplus1_1),
  ("test_saconplus1_2", `Quick, test_saconplus1_2),
  ("test_saconplus1_3_hole", `Quick, test_saconplus1_3_hole),
  ("test_saconplus1_4_compound", `Quick, test_saconplus1_4_compound),
  ("test_saconplus2_1", `Quick, test_saconplus2_1),
  ("test_saconplus2_2", `Quick, test_saconplus2_2),
];
