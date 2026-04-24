open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

// Analytic action rules (Hazelnut 2017 Appendix A.3.3):
//   AASubsume, AAMove, AADel, AAConAsc, AAConVar, AAConLam1, AAConLam2,
//   AAConNumLit, AAFinish, AAZipLam.
//
// AASubsume delegates any not-directly-handled action to the synthetic
// judgement; these tests deliberately put cursors inside analytic positions
// (lambda bodies, etc.) so the AAZipLam zipper path is exercised as well.

// ==================================================================
// AAMove — mirrors SAMove but under ana.
// ==================================================================

// Movement into a lambda body ignores the ana type (movement doesn't touch it).
let test_aamove_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Lam("f", Plus(Lit(1), Lit(2))));
  let a: Hazelnut.Action.t = Move(Child(One));
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(Hazelnut.Zexp.Lam("f", Cursor(Plus(Lit(1), Lit(2)))));
  check(zexp_typ, "same", given, expected);
};

// Movement into the argument of an application.
let test_aamove_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = Cursor(Ap(Lam("f", Lit(1)), Var("x")));
  let a: Hazelnut.Action.t = Move(Child(Two));
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(Hazelnut.Zexp.RAp(Lam("f", Lit(1)), Cursor(Var("x"))));
  check(zexp_typ, "same", given, expected);
};

// Move(Parent) out of a type cursor in an ascription.
let test_aamove_3 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("f", Lit(1)), Cursor(Arrow(Num, Num)));
  let a: Hazelnut.Action.t = Move(Parent);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(Hazelnut.Zexp.Cursor(Asc(Lam("f", Lit(1)), Arrow(Num, Num))));
  check(zexp_typ, "same", given, expected);
};

// Move inside a lambda body (via AAZipLam).
let test_aamove_4_zip_lam = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    Lam("x", Cursor(Plus(Lit(1), Plus(Lit(2), Lit(3)))));
  let a: Hazelnut.Action.t = Move(Child(One));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(
      Hazelnut.Zexp.Lam(
        "x",
        LPlus(Cursor(Lit(1)), Plus(Lit(2), Lit(3))),
      ),
    );
  check(zexp_typ, "same", given, expected);
};

// ==================================================================
// AADel: ▶ė◀ --del--> ▶∅◀ (ana context)
// ==================================================================

let test_aadel_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("f", Lit(1)), Cursor(Arrow(Num, Num)));
  let a: Hazelnut.Action.t = Del;
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(Hazelnut.Zexp.RAsc(Lam("f", Lit(1)), Cursor(Hole)));
  check(zexp_typ, "same", given, expected);
};

// Del from within a type cursor inside an arrow.
let test_aadel_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t =
    RAsc(Lam("f", Plus(Lit(1), Var("x"))), RArrow(Num, Cursor(Num)));
  let a: Hazelnut.Action.t = Del;
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(
      Hazelnut.Zexp.RAsc(
        Lam("f", Plus(Lit(1), Var("x"))),
        RArrow(Num, Cursor(Hole)),
      ),
    );
  check(zexp_typ, "same", given, expected);
};

// Del applied through AAZipLam: inside a lambda body analyzed at Num.
let test_aadel_3_zip_lam = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    Lam("x", Cursor(Plus(Lit(1), Lit(2))));
  let a: Hazelnut.Action.t = Del;
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.Lam("x", Cursor(EHole)));
  check(zexp_typ, "same", given, expected);
};

// ==================================================================
// AAConAsc: ▶ė◀ --construct asc--> ė : ▶τ̇◀ (τ̇ = analyzed type)
// Unlike SAConAsc, AAConAsc always succeeds and uses the analyzed type.
// ==================================================================

let test_aaconasc_1 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = Cursor(Var("x"));
  let a: Hazelnut.Action.t = Construct(Asc);
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.RAsc(Var("x"), Cursor(Num)));
  check(zexp_typ, "same", given, expected);
};

let test_aaconasc_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(Var("x"));
  let a: Hazelnut.Action.t = Construct(Asc);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(Hazelnut.Zexp.RAsc(Var("x"), Cursor(Arrow(Num, Num))));
  check(zexp_typ, "same", given, expected);
};

// AAConAsc on a compound subject (a Plus).
let test_aaconasc_3_compound = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(Plus(Lit(1), Plus(Lit(2), Lit(3))));
  let a: Hazelnut.Action.t = Construct(Asc);
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(
      Hazelnut.Zexp.RAsc(Plus(Lit(1), Plus(Lit(2), Lit(3))), Cursor(Num)),
    );
  check(zexp_typ, "same", given, expected);
};

// ==================================================================
// AAConVar: Γ, x:τ̇' ⊢ ▶∅◀ --construct var x--> ⦇▶x◀⦈ ⇐ τ̇
//   iff τ̇ INconsistent with τ̇'.
// If τ̇ consistent with τ̇', AASubsume delegates to SAConVar (no hole wrap).
// ==================================================================

// INconsistent: x has Arrow, τ is Num. Must wrap in NEHole.
let test_aaconvar_1 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Var("x"));
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.NEHole(Cursor(Var("x"))));
  check(zexp_typ, "same", given, expected);
};

// Consistent: x has Num, τ is Num. Via AASubsume → SAConVar, no wrap.
let test_aaconvar_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Var("x"));
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.Cursor(Var("x")));
  check(zexp_typ, "same", given, expected);
};

// Hole-typed variable is consistent with anything (via AASubsume).
let test_aaconvar_3_hole_type = () => {
  let ctx: typctx = TypCtx.singleton("h", Hazelnut.Htyp.Hole);
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Var("h"));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.Cursor(Var("h")));
  check(zexp_typ, "same", given, expected);
};

// Arrow consistent with Arrow via TCArr.
let test_aaconvar_4_arrow_consistent = () => {
  let ctx: typctx =
    TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Hole, Num));
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Var("f"));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.Cursor(Var("f")));
  check(zexp_typ, "same", given, expected);
};

// If the variable is unbound, the action fails (no rule applies).
let test_aaconvar_5_unbound_fails = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Var("z"));
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = None;
  check(zexp_typ, "same", given, expected);
};

// ==================================================================
// AAConLam1: τ̇ ▸→ (τ̇₁→τ̇₂)  =>  ▶∅◀ --construct lam x--> (λx.▶∅◀) ⇐ τ̇
// AAConLam2: τ̇ ⌿▸→          =>  ▶∅◀ --construct lam x--> ⦇(λx.∅):(▶⦇⦈◀→⦇⦈)⦈ ⇐ τ̇
// ==================================================================

// AAConLam1: τ = Arrow(Num, Num) — matches arrow directly.
let test_aaconlam1_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lam("x"));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.Lam("x", Cursor(EHole)));
  check(zexp_typ, "same", given, expected);
};

// AAConLam1: τ = Arrow(Hole, Hole) — still AAConLam1.
let test_aaconlam1_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lam("x"));
  let ht: Hazelnut.Htyp.t = Arrow(Hole, Hole);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.Lam("x", Cursor(EHole)));
  check(zexp_typ, "same", given, expected);
};

// AAConLam1 via MAHole: τ = Hole.
let test_aaconlam1_3_mahole = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lam("y"));
  let ht: Hazelnut.Htyp.t = Hole;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.Lam("y", Cursor(EHole)));
  check(zexp_typ, "same", given, expected);
};

// AAConLam2: τ = Num, doesn't match arrow — wraps in NEHole.
let test_aaconlam2_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lam("x"));
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(
      Hazelnut.Zexp.NEHole(
        RAsc(Lam("x", EHole), LArrow(Cursor(Hole), Hole)),
      ),
    );
  check(zexp_typ, "same", given, expected);
};

let test_aaconlam2_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lam("y"));
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(
      Hazelnut.Zexp.NEHole(
        RAsc(Lam("y", EHole), LArrow(Cursor(Hole), Hole)),
      ),
    );
  check(zexp_typ, "same", given, expected);
};

// ==================================================================
// AAConNumLit: τ̇ ⌿~ num => ▶∅◀ --construct n--> ⦇▶n◀⦈ ⇐ τ̇
// If τ̇ ~ num, AASubsume → SAConNumLit (no wrap).
// ==================================================================

let test_aaconnumlit_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lit(1));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.NEHole(Cursor(Lit(1))));
  check(zexp_typ, "same", given, expected);
};

let test_aaconnumlit_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lit(-1));
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.Cursor(Lit(-1)));
  check(zexp_typ, "same", given, expected);
};

// At Hole: Hole ~ num, so consistent path (via AASubsume).
let test_aaconnumlit_3_hole = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(EHole);
  let a: Hazelnut.Action.t = Construct(Lit(7));
  let ht: Hazelnut.Htyp.t = Hole;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.Cursor(Lit(7)));
  check(zexp_typ, "same", given, expected);
};

// ==================================================================
// AAFinish: ▶⦇ė⦈◀ --finish--> ▶ė◀ (when Γ ⊢ ė ⇐ τ̇)
// ==================================================================

let test_aafinish_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Cursor(NEHole(Plus(Lit(1), Lit(1))));
  let a: Hazelnut.Action.t = Finish;
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.Cursor(Plus(Lit(1), Lit(1))));
  check(zexp_typ, "same", given, expected);
};

// Finishing a lambda at an arrow type (ALam).
let test_aafinish_2_lam_arrow = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let ze: Hazelnut.Zexp.t =
    Cursor(NEHole(Lam("f", Plus(Var("x"), Var("x")))));
  let a: Hazelnut.Action.t = Finish;
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(Hazelnut.Zexp.Cursor(Lam("f", Plus(Var("x"), Var("x")))));
  check(zexp_typ, "same", given, expected);
};

// Finishing a lambda at Num must fail (ALam needs an arrow, ASubsume fails).
let test_aafinish_3_fail_lam_num = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t =
    Cursor(NEHole(Lam("x", Lit(1))));
  let a: Hazelnut.Action.t = Finish;
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = None;
  check(zexp_typ, "same", given, expected);
};

// ==================================================================
// AAZipLam — the KEY analytic zipper: τ ▸→ (τ₁→τ₂), Γ,x:τ₁ ⊢ ê --α--> ê' ⇐ τ₂
// ==================================================================

// Construct plus inside a lambda body analyzed at Arrow(Num, Num).
let test_aazip_lam_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Lam("x", Cursor(Lit(5)));
  let a: Hazelnut.Action.t = Construct(Plus);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(Hazelnut.Zexp.Lam("x", RPlus(Lit(5), Cursor(EHole))));
  check(zexp_typ, "same", given, expected);
};

// AAZipLam through MAHole — τ=Hole, so body analyzed at Hole with x:Hole.
let test_aazip_lam_2_mahole = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Lam("x", Cursor(Var("x")));
  let a: Hazelnut.Action.t = Construct(Plus);
  let ht: Hazelnut.Htyp.t = Hole;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected =
    Some(Hazelnut.Zexp.Lam("x", RPlus(Var("x"), Cursor(EHole))));
  check(zexp_typ, "same", given, expected);
};

// AAZipLam: action builds an application inside the body.
let test_aazip_lam_3_construct_ap = () => {
  let ctx: typctx = TypCtx.empty;
  // Lambda ⇐ (Num→Num) → Num; body f, being applied with Ap.
  let ze: Hazelnut.Zexp.t = Lam("f", Cursor(Var("f")));
  let a: Hazelnut.Action.t = Construct(Ap);
  let ht: Hazelnut.Htyp.t = Arrow(Arrow(Num, Num), Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  // Body is Cursor(Var("f")), under ana at Num. AASubsume → syn: Var("f")
  // syns Arrow(Num,Num). Construct(Ap) via SAConApArr: f(▶∅◀) : Num. Then
  // Num ~ Num is fine.
  let expected =
    Some(Hazelnut.Zexp.Lam("f", RAp(Var("f"), Cursor(EHole))));
  check(zexp_typ, "same", given, expected);
};

// AAZipLam fails when τ doesn't match arrow.
let test_aazip_lam_4_fail_not_arrow = () => {
  let ctx: typctx = TypCtx.empty;
  let ze: Hazelnut.Zexp.t = Lam("x", Cursor(Lit(5)));
  let a: Hazelnut.Action.t = Construct(Plus);
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = None;
  check(zexp_typ, "same", given, expected);
};

// ==================================================================
// AASubsume — delegates to syn_action when the action isn't directly
// matched by one of AAConAsc/AAConVar/AAConLam/AAConNumLit/AAFinish/AAZipLam.
// e.g. Construct(Ap) on a synthesizing expression under ana.
// ==================================================================

// AASubsume: Construct(Ap) on Cursor(Var("f")) under ana at Num.
// Var f syns Arrow(Num,Num); SAConApArr gives RAp(Var f, Cursor(EHole)) : Num.
// Num ~ Num, consistent.
let test_aasubsume_1_con_ap = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(Var("f"));
  let a: Hazelnut.Action.t = Construct(Ap);
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.RAp(Var("f"), Cursor(EHole)));
  check(zexp_typ, "same", given, expected);
};

// AASubsume: syn_action would produce a result whose type is INconsistent
// with the analyzed type — the ana_action fails.
// Cursor(Var("f")) with f:Arrow(Num, Num), action Construct(Plus), analyzed
// at Arrow(Num, Num). SAConPlus2 produces (⦇f⦈+▶∅◀):Num, τ=Arrow(Num,Num)~Num
// is FALSE, so AASubsume fails. There's no direct AA rule for Plus, so overall
// fails.
let test_aasubsume_2_fail_inconsistent = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(Var("f"));
  let a: Hazelnut.Action.t = Construct(Plus);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = None;
  check(zexp_typ, "same", given, expected);
};

// AASubsume over a Del action inside an Ap (via SAZipApAna then AASubsume).
let test_aasubsume_3_del_in_ap = () => {
  let ctx: typctx = TypCtx.singleton("f", Hazelnut.Htyp.Arrow(Num, Num));
  let ze: Hazelnut.Zexp.t = Cursor(Ap(Var("f"), Lit(1)));
  let a: Hazelnut.Action.t = Move(Child(Two));
  let ht: Hazelnut.Htyp.t = Num;
  let given = Hazelnut.ana_action(ctx, ze, a, ht);
  let expected = Some(Hazelnut.Zexp.RAp(Var("f"), Cursor(Lit(1))));
  check(zexp_typ, "same", given, expected);
};

let ana_action_tests = [
  // Move
  ("test_aamove_1", `Quick, test_aamove_1),
  ("test_aamove_2", `Quick, test_aamove_2),
  ("test_aamove_3", `Quick, test_aamove_3),
  ("test_aamove_4_zip_lam", `Quick, test_aamove_4_zip_lam),
  // Del
  ("test_aadel_1", `Quick, test_aadel_1),
  ("test_aadel_2", `Quick, test_aadel_2),
  ("test_aadel_3_zip_lam", `Quick, test_aadel_3_zip_lam),
  // Construct Asc
  ("test_aaconasc_1", `Quick, test_aaconasc_1),
  ("test_aaconasc_2", `Quick, test_aaconasc_2),
  ("test_aaconasc_3_compound", `Quick, test_aaconasc_3_compound),
  // Construct Var
  ("test_aaconvar_1", `Quick, test_aaconvar_1),
  ("test_aaconvar_2", `Quick, test_aaconvar_2),
  ("test_aaconvar_3_hole_type", `Quick, test_aaconvar_3_hole_type),
  (
    "test_aaconvar_4_arrow_consistent",
    `Quick,
    test_aaconvar_4_arrow_consistent,
  ),
  ("test_aaconvar_5_unbound_fails", `Quick, test_aaconvar_5_unbound_fails),
  // Construct Lam
  ("test_aaconlam1_1", `Quick, test_aaconlam1_1),
  ("test_aaconlam1_2", `Quick, test_aaconlam1_2),
  ("test_aaconlam1_3_mahole", `Quick, test_aaconlam1_3_mahole),
  ("test_aaconlam2_1", `Quick, test_aaconlam2_1),
  ("test_aaconlam2_2", `Quick, test_aaconlam2_2),
  // Construct NumLit
  ("test_aaconnumlit_1", `Quick, test_aaconnumlit_1),
  ("test_aaconnumlit_2", `Quick, test_aaconnumlit_2),
  ("test_aaconnumlit_3_hole", `Quick, test_aaconnumlit_3_hole),
  // Finish
  ("test_aafinish_1", `Quick, test_aafinish_1),
  ("test_aafinish_2_lam_arrow", `Quick, test_aafinish_2_lam_arrow),
  ("test_aafinish_3_fail_lam_num", `Quick, test_aafinish_3_fail_lam_num),
  // AAZipLam
  ("test_aazip_lam_1", `Quick, test_aazip_lam_1),
  ("test_aazip_lam_2_mahole", `Quick, test_aazip_lam_2_mahole),
  ("test_aazip_lam_3_construct_ap", `Quick, test_aazip_lam_3_construct_ap),
  ("test_aazip_lam_4_fail_not_arrow", `Quick, test_aazip_lam_4_fail_not_arrow),
  // AASubsume
  ("test_aasubsume_1_con_ap", `Quick, test_aasubsume_1_con_ap),
  (
    "test_aasubsume_2_fail_inconsistent",
    `Quick,
    test_aasubsume_2_fail_inconsistent,
  ),
  ("test_aasubsume_3_del_in_ap", `Quick, test_aasubsume_3_del_in_ap),
];
