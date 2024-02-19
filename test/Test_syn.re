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
    Asc(Lam("x", Plus(Lit(1), Lit(2))), Arrow(Num, Num));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) =
    Some(Hazelnut.Htyp.Arrow(Num, Num));
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

let test_sasc_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let he: Hazelnut.Hexp.t = Asc(NEHole(Var("x")), Hole);
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

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

let test_sap_1 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = Ap(Var("x"), Lit(1));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

let test_sap_2 = () => {
  let ctx: typctx =
    TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Arrow(Num, Num), Num));
  let he: Hazelnut.Hexp.t = Ap(Var("x"), Lam("y", Lit(1)));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

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

let test_shole_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = EHole;
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

let test_shole_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Hole);
  let he: Hazelnut.Hexp.t = Var("x");
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

let test_snehole_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = NEHole(EHole);
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

let test_snehole_2 = () => {
  let ctx: typctx = TypCtx.singleton("incr", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = NEHole(Asc(Ap(Var("incr"), Lit(1)), Num));
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Hole);
  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
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
  ("test_snehole_1", `Quick, test_snehole_1),
  ("test_snehole_2", `Quick, test_snehole_2),
];
