open Alcotest;
open Test_interface;
module Hazelnut = Hazelnut_lib.Hazelnut;

// adding context definition here because importing from Hazelnut gives an unbound error
// TODO: fix this
module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

let test_sasc_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Var("x");
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = None;

  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

let test_sasc_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let he: Hazelnut.Hexp.t = Var("x");
  let given: option(Hazelnut.Htyp.t) = Hazelnut.syn(ctx, he);
  let expected: option(Hazelnut.Htyp.t) = Some(Hazelnut.Htyp.Num);

  check(htyp_typ, "same option(Hazelnut.Htyp.t)", given, expected);
};

let syn_tests = [
  ("test_sasc_1", `Quick, test_sasc_1),
  ("test_sasc_2", `Quick, test_sasc_2),
];
