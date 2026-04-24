open Sexplib.Std;
// open Monad_lib.Monad; // Uncomment to use the let*/let+ monad syntax

let compare_string = String.compare;
let compare_int = Int.compare;

module Htyp = {
  [@deriving (sexp, compare, show({with_path: false}))]
  type t =
    | Arrow(t, t)
    | Num
    | Hole;
};

module Hexp = {
  [@deriving (sexp, compare, show({with_path: false}))]
  type t =
    | Var(string)
    | Lam(string, t)
    | Ap(t, t)
    | Lit(int)
    | Plus(t, t)
    | Asc(t, Htyp.t)
    | EHole
    | NEHole(t);
};

module Ztyp = {
  [@deriving (sexp, compare, show({with_path: false}))]
  type t =
    | Cursor(Htyp.t)
    | LArrow(t, Htyp.t)
    | RArrow(Htyp.t, t);
};

module Zexp = {
  [@deriving (sexp, compare, show({with_path: false}))]
  type t =
    | Cursor(Hexp.t)
    | Lam(string, t)
    | LAp(t, Hexp.t)
    | RAp(Hexp.t, t)
    | LPlus(t, Hexp.t)
    | RPlus(Hexp.t, t)
    | LAsc(t, Htyp.t)
    | RAsc(Hexp.t, Ztyp.t)
    | NEHole(t);
};

module Child = {
  [@deriving (sexp, compare)]
  type t =
    | One
    | Two;
};

module Dir = {
  [@deriving (sexp, compare)]
  type t =
    | Child(Child.t)
    | Parent;
};

module Shape = {
  [@deriving (sexp, compare)]
  type t =
    | Arrow
    | Num
    | Asc
    | Var(string)
    | Lam(string)
    | Ap
    | Lit(int)
    | Plus
    | NEHole;
};

module Action = {
  [@deriving (sexp, compare)]
  type t =
    | Move(Dir.t)
    | Construct(Shape.t)
    | Del
    | Finish;
};

module TypCtx = Map.Make(String);
type typctx = TypCtx.t(Htyp.t);

exception Unimplemented;

// =====================================================================
// STEP 1: Type compatibility helpers (Definition 1-3 in the paper)
//
// These helpers are used pervasively by the typing and action rules.
// Implement them first — the test suite can verify them in isolation.
// =====================================================================

// Type consistency (Definition 1, Figure 9):
//   TCRefl:  τ ~ τ                    (reflexivity for base types)
//   TCHole1: ⦇⦈ ~ τ                   (hole is consistent with anything)
//   TCHole2: τ ~ ⦇⦈                   (symmetric)
//   TCArr:   τ₁ ~ τ₁' ∧ τ₂ ~ τ₂'  →  (τ₁→τ₂) ~ (τ₁'→τ₂')
let consistent = (t1: Htyp.t, t2: Htyp.t): bool => {
  let _ = (t1, t2);
  raise(Unimplemented);
};

// Type inconsistency (Definition 2): τ ⌿~ τ'  iff  ¬(τ ~ τ')
let inconsistent = (t1: Htyp.t, t2: Htyp.t): bool => {
  let _ = (t1, t2);
  raise(Unimplemented);
};

// Matched arrow types (Definition 3, Figure 9):
//   MAArr:  (τ₁→τ₂) ▸→ (τ₁, τ₂)
//   MAHole: ⦇⦈ ▸→ (⦇⦈, ⦇⦈)
let matched_arrow = (t: Htyp.t): option((Htyp.t, Htyp.t)) => {
  let _ = t;
  raise(Unimplemented);
};

// =====================================================================
// STEP 2: Cursor erasure (Appendix A.2)
//
// These strip the cursor position from a zipper, recovering the
// underlying H-type or H-expression. Needed by the action semantics
// and the webapp's theorem checks.
// =====================================================================

// Type cursor erasure (Appendix A.2.1):
//   ETTop:   erase(▶τ̇◀)   = τ̇
//   ETArrL:  erase(τ̂ → τ̇) = erase(τ̂) → τ̇
//   ETArrR:  erase(τ̇ → τ̂) = τ̇ → erase(τ̂)
let erase_typ = (zt: Ztyp.t): Htyp.t =>
  switch (zt) {
  | Cursor(_) => raise(Unimplemented)   // ETTop
  | LArrow(_, _) => raise(Unimplemented) // ETArrL
  | RArrow(_, _) => raise(Unimplemented) // ETArrR
  };

// Expression cursor erasure (Appendix A.2.2):
//   EETop:    erase(▶ė◀)   = ė
//   EEAscL:   erase(ê : τ̇) = erase(ê) : τ̇
//   EEAscR:   erase(ė : τ̂) = ė : erase_typ(τ̂)
//   EELam:    erase(λx.ê)  = λx.erase(ê)
//   EEApL:    erase(ê ė)   = erase(ê) ė
//   EEApR:    erase(ė ê)   = ė erase(ê)
//   EEPlusL:  erase(ê + ė) = erase(ê) + ė
//   EEPlusR:  erase(ė + ê) = ė + erase(ê)
//   EENEHole: erase(⦇ê⦈)   = ⦇erase(ê)⦈
let erase_exp = (ze: Zexp.t): Hexp.t =>
  switch (ze) {
  | Cursor(_) => raise(Unimplemented)    // EETop
  | LAsc(_, _) => raise(Unimplemented)   // EEAscL
  | RAsc(_, _) => raise(Unimplemented)   // EEAscR
  | Lam(_, _) => raise(Unimplemented)    // EELam
  | LAp(_, _) => raise(Unimplemented)    // EEApL
  | RAp(_, _) => raise(Unimplemented)    // EEApR
  | LPlus(_, _) => raise(Unimplemented)  // EEPlusL
  | RPlus(_, _) => raise(Unimplemented)  // EEPlusR
  | NEHole(_) => raise(Unimplemented)    // EENEHole
  };

// =====================================================================
// STEP 3: Bidirectional type system (Section 3.1, Figure 9)
//
// syn and ana are mutually recursive. syn returns the synthesized type
// (or None if the expression is ill-typed); ana returns whether the
// expression checks against the given type.
// =====================================================================

// Type synthesis — Γ ⊢ e ⇒ τ:
//   SVar    (1a): Γ(x) = τ                               →  Γ ⊢ x ⇒ τ
//   SAp     (1b): Γ ⊢ e₁ ⇒ τ₁, τ₁ ▸→ (τ₂,τ), Γ ⊢ e₂ ⇐ τ₂  →  Γ ⊢ e₁(e₂) ⇒ τ
//   SNum    (1c):                                         →  Γ ⊢ n ⇒ num
//   SPlus   (1d): Γ ⊢ e₁ ⇐ num, Γ ⊢ e₂ ⇐ num          →  Γ ⊢ e₁+e₂ ⇒ num
//   SAsc    (1e): Γ ⊢ e ⇐ τ                              →  Γ ⊢ (e : τ) ⇒ τ
//   SEHole  (1f):                                         →  Γ ⊢ ⦇⦈ ⇒ ⦇⦈
//   SNEHole (1g): Γ ⊢ e ⇒ τ                              →  Γ ⊢ ⦇e⦈ ⇒ ⦇⦈
//
// Note: Lam has NO synthesis rule — it can only be checked analytically.
let syn = (ctx: typctx, e: Hexp.t): option(Htyp.t) => {
  let _ = (ctx, e);
  raise(Unimplemented);
}

// Type analysis — Γ ⊢ e ⇐ τ:
//   ALam      (2a): τ ▸→ (τ₁,τ₂), Γ,x:τ₁ ⊢ e ⇐ τ₂    →  Γ ⊢ λx.e ⇐ τ
//   ASubsume  (2b): Γ ⊢ e ⇒ τ', τ ~ τ'                  →  Γ ⊢ e ⇐ τ
and ana = (ctx: typctx, e: Hexp.t, t: Htyp.t): bool => {
  let _ = (ctx, e, t);
  raise(Unimplemented);
};

// =====================================================================
// STEP 4: Action semantics (Section 3.3, Figures 10-11)
//
// This is the heart of Hazelnut. You will need to implement several
// internal helpers before tackling the main action functions:
//
//   type_action : (Ztyp.t, Action.t) → option(Ztyp.t)
//     Type actions: move, delete, and construct on type zippers.
//     Rules: TMArrChild1/2, TMArrParent1/2, TMDel, TConArrow, TConNum,
//            TMArrZip1, TMArrZip2
//
//   move_exp : (Zexp.t, Dir.t) → option(Zexp.t)
//     Expression movement: move the cursor up/down the tree.
//     Handle each Zexp form's Child(One)/Child(Two)/Parent cases,
//     plus zipper recursion (delegate to type_action for RAsc).
//
// A suggested implementation order within the actions:
//   1. Movement (Move)    — simplest, no type changes
//   2. Deletion (Del)     — replaces cursor target with a hole
//   3. Construction       — the bulk of the rules
//   4. Finishing (Finish) — unwraps non-empty holes
//   5. Zipper rules       — recursive propagation through the tree
// =====================================================================

// Synthetic action — Γ ⊢ ê ⇒ τ --α--> ê' ⇒ τ':
//
// Base cases on (ê, α):
//   SAMove        (7a):  (ê, move δ) → move cursor
//   SADel         (13a): (▶ė◀, del)  → ▶⦇⦈◀ ⇒ ⦇⦈
//   SAConAsc      (13c): (▶ė◀, construct asc) → ė : ▶τ̇◀
//   SAConVar      (13e): (▶⦇⦈◀, construct var x) → ▶x◀ ⇒ Γ(x)
//   SAConLam      (13g): (▶⦇⦈◀, construct lam x) → λx.⦇⦈ : ▶⦇⦈◀→⦇⦈
//   SAConApArr    (13h): (▶ė◀, construct ap) when τ ▸→ → ė(▶⦇⦈◀)
//   SAConApOtw    (13i): (▶ė◀, construct ap) when τ ⌿~ (⦇⦈→⦇⦈) → ⦇ė⦈(▶⦇⦈◀)
//   SAConNumLit   (13j): (▶⦇⦈◀, construct lit n) → ▶n◀ ⇒ num
//   SAConPlus1    (13k): (▶ė◀, construct plus) when τ ~ num → ė + ▶⦇⦈◀
//   SAConPlus2    (13l): (▶ė◀, construct plus) when τ ⌿~ num → ⦇ė⦈ + ▶⦇⦈◀
//   SAConNEHole   (13m): (▶ė◀, construct nehole) → ⦇▶ė◀⦈
//   SAFinish      (15a): (▶⦇ė⦈◀, finish) → ▶ė◀ ⇒ syn(ė)
//
// Zipper cases (action propagates into a subterm):
//   SAZipAsc1     (18b): LAsc(ê, τ̇)  — action on expression in ascription
//   SAZipAsc2     (18c): RAsc(ė, τ̂)  — action on type in ascription
//   SAZipApArr    (18d): LAp(ê, ė)   — action on function in application
//   SAZipApAna    (18e): RAp(ė, ê)   — action on argument in application
//   SAZipPlus1    (18f): LPlus(ê, ė) — action on left of plus
//   SAZipPlus2    (18g): RPlus(ė, ê) — action on right of plus
//   SAZipHole     (18h): NEHole(ê)   — action inside non-empty hole
let syn_action =
        (ctx: typctx, (ze: Zexp.t, t: Htyp.t), a: Action.t)
        : option((Zexp.t, Htyp.t)) => {
  let _ = (ctx, ze, t, a);
  raise(Unimplemented);
}

// Analytic action — Γ ⊢ ê --α--> ê' ⇐ τ:
//
// Base cases:
//   AAMove        (7b):  (ê, move δ) → move cursor
//   AADel         (5):   (▶ė◀, del)  → ▶⦇⦈◀
//   AAConAsc      (13b): (▶ė◀, construct asc) → ė : ▶τ̇◀
//   AAConVar      (13d): (▶⦇⦈◀, construct var x) — subsume or wrap in NEHole
//   AAConLam1     (13e): (▶⦇⦈◀, construct lam x) when τ ▸→  → λx.▶⦇⦈◀
//   AAConLam2     (13f): (▶⦇⦈◀, construct lam x) when τ ⌿~ (⦇⦈→⦇⦈)
//   AAConNumLit   (13k): (▶⦇⦈◀, construct lit n) — subsume or wrap in NEHole
//   AAFinish      (15b): (▶⦇ė⦈◀, finish) when Γ ⊢ e ⇐ τ → ▶ė◀
//
// Zipper case:
//   AAZipLam      (18a): Lam(x, ê) — action inside lambda body
//
// Subsumption fallthrough:
//   AASubsume     (16b): if none of the above match, try syn_action
//                        and check that the result type is consistent with τ.
and ana_action =
    (ctx: typctx, ze: Zexp.t, a: Action.t, t: Htyp.t): option(Zexp.t) => {
  let _ = (ctx, ze, a, t);
  raise(Unimplemented);
};
