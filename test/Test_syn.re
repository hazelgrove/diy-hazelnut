open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

// adding context definition here because importing from Hazelnut gives an unbound error
// TODO: fix this
module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

let test_sasc_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t =
    Asc(Lam("x", Num, Plus(NumLit(1), NumLit(2))), Arrow(Num, Num));
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected: Hazelnut.Htyp.t = Hazelnut.Htyp.Arrow(Num, Num);
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_sasc_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let he: Hazelnut.Hexp.t = Asc(Var("x"), Hole);
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hazelnut.Htyp.Hole;
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_svar_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Var("x");
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hole;
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_svar_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let he: Hazelnut.Hexp.t = Var("x");
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hazelnut.Htyp.Num;
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_sap_1 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = Ap(Var("x"), NumLit(1));
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hazelnut.Htyp.Num;
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_sap_2 = () => {
  let ctx: typctx =
    TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Arrow(Num, Num), Num));
  let he: Hazelnut.Hexp.t = Ap(Var("x"), Lam("y",Num, NumLit(1)));
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hazelnut.Htyp.Num;
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_snum_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = NumLit(1);
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hazelnut.Htyp.Num;
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_snum_2 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = NumLit(-1);
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hazelnut.Htyp.Num;
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_splus_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Plus(NumLit(1), NumLit(-1));
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hazelnut.Htyp.Num;
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_splus_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let he: Hazelnut.Hexp.t = Plus(NumLit(1), Var("x"));
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hazelnut.Htyp.Num;
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_shole_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = EHole;
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hazelnut.Htyp.Hole;
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_shole_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Hole);
  let he: Hazelnut.Hexp.t = Var("x");
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hazelnut.Htyp.Hole;
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_smark_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Mark(Var("x"), Free);
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hazelnut.Htyp.Hole;
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let test_smark_2 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Num); // non-arrow for mark
  let he: Hazelnut.Hexp.t = Mark(Asc(Ap(Var("incr"), NumLit(1)), Num), NonArrowAp);
  let (_, given): (Hazelnut.Hexp.t, Hazelnut.Htyp.t) = Hazelnut.mark_syn(ctx, he);
  let expected:(Hazelnut.Htyp.t) = Hazelnut.Htyp.Num; // TODO: expected behavior?
  check(htyp_typ, "same Hazelnut.Htyp.t", given, expected);
};

let syn_tests = [
  ("test_sasc_1", `Quick, test_sasc_1),
  ("test_sasc_2", `Quick, test_sasc_2),
  ("test_svar_1", `Quick, test_svar_1),
  ("test_svar_2", `Quick, test_svar_2),
  ("test_sap_1", `Quick, test_sap_1),
  ("test_sap_2", `Quick, test_sap_2),
  ("test_snum_1", `Quick, test_snum_1),
  ("test_snum_2", `Quick, test_snum_2),
  ("test_splus_1", `Quick, test_splus_1),
  ("test_splus_2", `Quick, test_splus_2),
  ("test_shole_1", `Quick, test_shole_1),
  ("test_shole_2", `Quick, test_shole_2),
  ("test_smark_1", `Quick, test_smark_1),
  ("test_smark_2", `Quick, test_smark_2),
];
