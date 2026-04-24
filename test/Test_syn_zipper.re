open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

// Synthetic zipper rules (Hazelnut 2017 Appendix A.3.3).
//
// These rules propagate actions through the zipper to reach the cursor.
// They are the trickiest part of the action semantics and a common source
// of student bugs — especially when the inner expressions are compound
// rather than simple values.
//
// Rule coverage:
//   SAZIPASC1  — cursor in expression position of an ascription
//   SAZIPASC2  — cursor in type position of an ascription (re-checks body)
//   SAZIPAPARR — cursor in function position of an application
//   SAZIPAPANA — cursor in argument position of an application
//   SAZIPPLUS1 — cursor in left operand of plus
//   SAZIPPLUS2 — cursor in right operand of plus
//   SAZIPHOLE  — cursor inside a non-empty hole

// ==================================================================
// SAZIPASC1: Γ ⊢ ê --α--> ê' ⇐ τ
//         => Γ ⊢ ê:τ --α--> ê':τ ⇒ τ
// The cursor is in the expression part of an ascription. The action is
// dispatched analytically against the ascription's type.
// ==================================================================

// Del inside expression of an ascription.
let test_sazipasc1_del = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    LAsc(Cursor(Plus(Lit(1), Lit(2))), Num);
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Del;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.LAsc(Cursor(EHole), Num), Hazelnut.Htyp.Num));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Var) inside expression of an ascription (via analytic dispatch).
let test_sazipasc1_convar = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t =
    LAsc(Cursor(EHole), Num);
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Var("x"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((Hazelnut.Zexp.LAsc(Cursor(Var("x")), Num), Hazelnut.Htyp.Num));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Lam) inside ascription at Arrow type: AAConLam1 kicks in.
let test_sazipasc1_conlam = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    LAsc(Cursor(EHole), Arrow(Num, Num));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Construct(Lam("x"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LAsc(Lam("x", Cursor(EHole)), Arrow(Num, Num)),
      Hazelnut.Htyp.Arrow(Num, Num),
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAZIPASC1 with cursor deep inside a lambda body (via AAZipLam).
let test_sazipasc1_deep_lam = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    LAsc(Lam("x", Cursor(EHole)), Arrow(Num, Num));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Construct(Lit(1));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LAsc(Lam("x", Cursor(Lit(1))), Arrow(Num, Num)),
      Hazelnut.Htyp.Arrow(Num, Num),
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAZIPASC1: construct var x inside lambda body of ascription; the lambda
// binds x, so we use x:Num from the arrow type, not from ctx.
let test_sazipasc1_bound_var = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    LAsc(Lam("x", Cursor(EHole)), Arrow(Num, Num));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Construct(Var("x"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LAsc(Lam("x", Cursor(Var("x"))), Arrow(Num, Num)),
      Hazelnut.Htyp.Arrow(Num, Num),
    ));
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAZIPASC2: τ̂ --α--> τ̂',  Γ ⊢ ė ⇐ τ̂'
//         => Γ ⊢ ė:τ̂ --α--> ė:τ̂' ⇒ τ̂'
// The cursor is in the type position of an ascription. After the type
// action, the expression must be RE-CHECKED against the new type.
// ==================================================================

// Construct(Arrow) on the type: changes Num to (Num → ⦇⦈).
// The expression Lam("x", Lit(1)) must re-check against Arrow(Num, Hole).
let test_sazipasc2_conarrow = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", Lit(1)), Cursor(Num));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Arrow);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAsc(Lam("x", Lit(1)), RArrow(Num, Cursor(Hole))),
      Hazelnut.Htyp.Arrow(Num, Hole),
    ));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Num) on a Hole type inside an arrow (via TMARRZIP2).
// Arrow(Num, Hole) becomes Arrow(Num, Num), expression must re-check.
let test_sazipasc2_connum_nested = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", Plus(Var("x"), Lit(1))), RArrow(Num, Cursor(Hole)));
  let t: Hazelnut.Htyp.t = Arrow(Num, Hole);
  let a: Hazelnut.Action.t = Construct(Num);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAsc(
        Lam("x", Plus(Var("x"), Lit(1))),
        RArrow(Num, Cursor(Num)),
      ),
      Hazelnut.Htyp.Arrow(Num, Num),
    ));
  check(zexp_htyp, "same", given, expected);
};

// Del type inside arrow in ascription: Arrow(Num, Num) → Arrow(Num, ⦇⦈).
// Re-check must succeed because the body can still ana at Hole.
let test_sazipasc2_del_in_arrow = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("x", Var("x")), RArrow(Num, Cursor(Num)));
  let t: Hazelnut.Htyp.t = Arrow(Num, Num);
  let a: Hazelnut.Action.t = Del;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAsc(Lam("x", Var("x")), RArrow(Num, Cursor(Hole))),
      Hazelnut.Htyp.Arrow(Num, Hole),
    ));
  check(zexp_htyp, "same", given, expected);
};

// SAZIPASC2: re-check FAILS because new type is incompatible with body.
// Body is Lam("x", Lit(1)), if we del the domain from Num to Hole
// in Arrow(⦇⦈, Num), that's fine. But if we try changing Arrow to Num
// via Del on the whole Arrow, then Lam can't ana at Hole... actually
// Lam CAN ana at Hole (via MAHole). Let me think of a true failure case.
// Actually SAZIPASC2 always succeeds IF the type action succeeds AND the
// re-check succeeds. The re-check can fail if the body can't analyze at
// the new type. Example: body is Plus(Lit(1), Lit(2)) which ana at Num.
// If we construct Arrow on Num, new type is Arrow(Num, ⦇⦈). Can Plus ana
// at Arrow(Num, ⦇⦈)? Plus syn Num, Arrow~Num is false. Lam? No lam here.
// So it should fail (return None).
let test_sazipasc2_recheck_fails = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Plus(Lit(1), Lit(2)), Cursor(Num));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Arrow);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected = None;
  check(zexp_htyp, "same", given, expected);
};

// SAZIPASC2 with compound expression: body is Asc(Lam("f", Ap(Var("f"), Lit(1))), Arrow(Num, Num)).
// Cursor on the outer ascription type.
let test_sazipasc2_compound_body = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lit(1), Cursor(Hole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Num);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAsc(Lit(1), Cursor(Num)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAZIPAPARR: cursor in function position of application
// Γ ⊢ ê ⇒ τ₂,  Γ ⊢ ê ⇒ τ₂ --α--> ê' ⇒ τ₃,  τ₃ ▸→ (τ₄→τ₅),  Γ ⊢ ė ⇐ τ₄
// => Γ ⊢ ê(ė) ⇒ τ₁ --α--> ê'(ė) ⇒ τ₅
//
// Key: the action on ê changes its type, so we must re-derive the
// matched arrow type and re-check the argument.
// ==================================================================

// Del function in f(1): erases f → EHole. EHole syns Hole.
// Hole ▸→ (Hole→Hole), arg Lit(1) ⇐ Hole: true. Result type: Hole.
let test_sazipaparr_del = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = LAp(Cursor(Var("f")), Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Del;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LAp(Cursor(EHole), Lit(1)),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Asc) on function: f → f : ▶(Num→Num)◀. Type unchanged.
let test_sazipaparr_conasc = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    LAp(Cursor(Var("f")), Plus(Lit(1), Lit(2)));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Asc);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LAp(
        RAsc(Var("f"), Cursor(Arrow(Num, Num))),
        Plus(Lit(1), Lit(2)),
      ),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Finish NEHole in function position: ⦇f⦈(1) → f(1).
// Before: ⦇f⦈ syns Hole ▸→ (Hole→Hole), after: f syns Arrow(Num,Num).
let test_sazipaparr_finish = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    LAp(Cursor(NEHole(Var("f"))), Plus(Lit(1), Lit(2)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Finish;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LAp(Cursor(Var("f")), Plus(Lit(1), Lit(2))),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Construct(NEHole) on function: wraps it in a hole. f(1) → ⦇f⦈(1).
// Result type should become Hole (since NEHole syns Hole ▸→ Hole→Hole).
let test_sazipaparr_connehole = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = LAp(Cursor(Var("f")), Lit(1));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(NEHole);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LAp(NEHole(Cursor(Var("f"))), Lit(1)),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Compound: cursor inside a nested application in function position.
// (f(1))(2): cursor on the 1 in Lit(1) in the inner arg, via SAZIPAPANA
// within the outer SAZIPAPARR.
let test_sazipaparr_nested = () => {
  let ctx: typctx =
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Arrow(Num, Num)));
  let ze: Hazelnut.Zexp.t =
    LAp(RAp(Var("f"), Cursor(Lit(1))), Lit(2));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Del;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LAp(RAp(Var("f"), Cursor(EHole)), Lit(2)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAZIPAPANA: cursor in argument position of application
// Γ ⊢ ė ⇒ τ₂,  τ₂ ▸→ (τ₃→τ₄),  Γ ⊢ ê --α--> ê' ⇐ τ₃
// => Γ ⊢ ė(ê) ⇒ τ₁ --α--> ė(ê') ⇒ τ₄
//
// The argument is dispatched analytically against the domain type.
// ==================================================================

// Del argument in f(▶1◀): cursor erases arg to EHole.
let test_sazipapana_del = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = RAp(Var("f"), Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Del;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAp(Var("f"), Cursor(EHole)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Plus) on arg: Lit(1) → (1 + ▶∅◀). Analytically at Num.
let test_sazipapana_conplus = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = RAp(Var("f"), Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Plus);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAp(Var("f"), RPlus(Lit(1), Cursor(EHole))),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Var) on EHole arg where var type is inconsistent with domain.
// f: Num→Num, arg analyzed at Num. g: Arrow(Num,Num), so g wrapped in NEHole.
let test_sazipapana_inconsistent_var = () => {
  let ctx: typctx =
    TypCtx.empty
    |> TypCtx.add("f", Hazelnut.Htyp.Arrow(Num, Num))
    |> TypCtx.add("g", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = RAp(Var("f"), Cursor(EHole));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Var("g"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAp(Var("f"), NEHole(Cursor(Var("g")))),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Lam) on EHole arg where domain is Arrow → AAConLam1 kicks in.
let test_sazipapana_conlam = () => {
  let ctx: typctx =
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Arrow(Num, Num), Num));
  let ze: Hazelnut.Zexp.t = RAp(Var("f"), Cursor(EHole));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Lam("x"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAp(Var("f"), Lam("x", Cursor(EHole))),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Compound: function is itself a Plus wrapped in a NEHole.
// Function: Hole (via MAHole, Hole→Hole). Arg analytically at Hole.
let test_sazipapana_hole_fn = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RAp(EHole, Cursor(EHole));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Lit(5));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RAp(EHole, Cursor(Lit(5))),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAZIPPLUS1: cursor in left operand of plus
// Γ ⊢ ê --α--> ê' ⇐ num
// => Γ ⊢ (ê + ė) ⇒ num --α--> (ê' + ė) ⇒ num
// ==================================================================

// Del left of plus: erases to EHole. EHole still ana at Num.
let test_sazipplus1_del = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    LPlus(Cursor(Plus(Lit(1), Lit(2))), Lit(3));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Del;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LPlus(Cursor(EHole), Lit(3)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Plus) on left operand: (▶1◀ + 2) → ((1 + ▶∅◀) + 2).
let test_sazipplus1_conplus = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = LPlus(Cursor(Lit(1)), Lit(2));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Plus);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LPlus(RPlus(Lit(1), Cursor(EHole)), Lit(2)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Var) in left of plus: compound right side.
let test_sazipplus1_convar = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t =
    LPlus(Cursor(EHole), Plus(Lit(2), Lit(3)));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Var("x"));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LPlus(Cursor(Var("x")), Plus(Lit(2), Lit(3))),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Left operand is itself a Plus with cursor deep inside.
let test_sazipplus1_nested = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    LPlus(RPlus(Lit(1), Cursor(Lit(2))), Lit(3));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Del;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.LPlus(RPlus(Lit(1), Cursor(EHole)), Lit(3)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAZIPPLUS2: cursor in right operand of plus
// Γ ⊢ ê --α--> ê' ⇐ num
// => Γ ⊢ (ė + ê) ⇒ num --α--> (ė + ê') ⇒ num
// ==================================================================

// Del right of plus.
let test_sazipplus2_del = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RPlus(Plus(Lit(1), Lit(2)), Cursor(Lit(3)));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Del;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RPlus(Plus(Lit(1), Lit(2)), Cursor(EHole)),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Lit) on right side's EHole.
let test_sazipplus2_conlit = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RPlus(Plus(Lit(1), Lit(2)), Cursor(EHole));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Lit(99));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RPlus(Plus(Lit(1), Lit(2)), Cursor(Lit(99))),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Plus) on right: (1 + ▶2◀) → (1 + (2 + ▶∅◀)).
let test_sazipplus2_conplus = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = RPlus(Lit(1), Cursor(Lit(2)));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Plus);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RPlus(Lit(1), RPlus(Lit(2), Cursor(EHole))),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Right operand is an Ap(f, x): compound non-value form.
let test_sazipplus2_compound = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t =
    RPlus(Lit(1), RAp(Var("f"), Cursor(EHole)));
  let t: Hazelnut.Htyp.t = Num;
  let a: Hazelnut.Action.t = Construct(Lit(2));
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.RPlus(Lit(1), RAp(Var("f"), Cursor(Lit(2)))),
      Hazelnut.Htyp.Num,
    ));
  check(zexp_htyp, "same", given, expected);
};

// ==================================================================
// SAZIPHOLE: cursor inside a non-empty hole
// Γ ⊢ ê ⇒ τ,  Γ ⊢ ê ⇒ τ --α--> ê' ⇒ τ'
// => Γ ⊢ ⦇ê⦈ ⇒ ⦇⦈ --α--> ⦇ê'⦈ ⇒ ⦇⦈
//
// Key: the inner action is dispatched SYNTHETICALLY (not analytically).
// The outer type is always Hole regardless of the inner type change.
// ==================================================================

// Del inside NEHole with compound expression.
let test_saziphole_del = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    NEHole(Cursor(Plus(Lit(1), Plus(Lit(2), Lit(3)))));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Del;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.NEHole(Cursor(EHole)),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Plus) on Lit inside NEHole: Lit(1) → (1 + ▶∅◀).
// Inner type changes from Num to Num. Outer stays Hole.
let test_saziphole_conplus = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Lit(1)));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Plus);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.NEHole(RPlus(Lit(1), Cursor(EHole))),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Construct(Ap) inside NEHole: f → f(▶∅◀). Inner type changes.
let test_saziphole_conap = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = NEHole(Cursor(Var("f")));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Ap);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.NEHole(RAp(Var("f"), Cursor(EHole))),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Deeply nested: NEHole inside NEHole, action on inner cursor.
let test_saziphole_nested = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = NEHole(NEHole(Cursor(Lit(1))));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Construct(Plus);
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.NEHole(NEHole(RPlus(Lit(1), Cursor(EHole)))),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

// Finish inside NEHole: removes inner hole. Outer stays as NEHole.
let test_saziphole_finish = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t =
    NEHole(Cursor(NEHole(Var("x"))));
  let t: Hazelnut.Htyp.t = Hole;
  let a: Hazelnut.Action.t = Finish;
  let given = Hazelnut.syn_action(ctx, (ze, t), a);
  let expected =
    Some((
      Hazelnut.Zexp.NEHole(Cursor(Var("x"))),
      Hazelnut.Htyp.Hole,
    ));
  check(zexp_htyp, "same", given, expected);
};

let syn_zipper_tests = [
  // SAZIPASC1
  ("test_sazipasc1_del", `Quick, test_sazipasc1_del),
  ("test_sazipasc1_convar", `Quick, test_sazipasc1_convar),
  ("test_sazipasc1_conlam", `Quick, test_sazipasc1_conlam),
  ("test_sazipasc1_deep_lam", `Quick, test_sazipasc1_deep_lam),
  ("test_sazipasc1_bound_var", `Quick, test_sazipasc1_bound_var),
  // SAZIPASC2
  ("test_sazipasc2_conarrow", `Quick, test_sazipasc2_conarrow),
  ("test_sazipasc2_connum_nested", `Quick, test_sazipasc2_connum_nested),
  ("test_sazipasc2_del_in_arrow", `Quick, test_sazipasc2_del_in_arrow),
  ("test_sazipasc2_recheck_fails", `Quick, test_sazipasc2_recheck_fails),
  ("test_sazipasc2_compound_body", `Quick, test_sazipasc2_compound_body),
  // SAZIPAPARR
  ("test_sazipaparr_del", `Quick, test_sazipaparr_del),
  ("test_sazipaparr_conasc", `Quick, test_sazipaparr_conasc),
  ("test_sazipaparr_finish", `Quick, test_sazipaparr_finish),
  ("test_sazipaparr_connehole", `Quick, test_sazipaparr_connehole),
  ("test_sazipaparr_nested", `Quick, test_sazipaparr_nested),
  // SAZIPAPANA
  ("test_sazipapana_del", `Quick, test_sazipapana_del),
  ("test_sazipapana_conplus", `Quick, test_sazipapana_conplus),
  (
    "test_sazipapana_inconsistent_var",
    `Quick,
    test_sazipapana_inconsistent_var,
  ),
  ("test_sazipapana_conlam", `Quick, test_sazipapana_conlam),
  ("test_sazipapana_hole_fn", `Quick, test_sazipapana_hole_fn),
  // SAZIPPLUS1
  ("test_sazipplus1_del", `Quick, test_sazipplus1_del),
  ("test_sazipplus1_conplus", `Quick, test_sazipplus1_conplus),
  ("test_sazipplus1_convar", `Quick, test_sazipplus1_convar),
  ("test_sazipplus1_nested", `Quick, test_sazipplus1_nested),
  // SAZIPPLUS2
  ("test_sazipplus2_del", `Quick, test_sazipplus2_del),
  ("test_sazipplus2_conlit", `Quick, test_sazipplus2_conlit),
  ("test_sazipplus2_conplus", `Quick, test_sazipplus2_conplus),
  ("test_sazipplus2_compound", `Quick, test_sazipplus2_compound),
  // SAZIPHOLE
  ("test_saziphole_del", `Quick, test_saziphole_del),
  ("test_saziphole_conplus", `Quick, test_saziphole_conplus),
  ("test_saziphole_conap", `Quick, test_saziphole_conap),
  ("test_saziphole_nested", `Quick, test_saziphole_nested),
  ("test_saziphole_finish", `Quick, test_saziphole_finish),
];
