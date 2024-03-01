open Alcotest;
module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);
type typctx = Hazelnut.TypCtx.t(Hazelnut.Htyp.t);

let test_asubsume_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Lit(1);
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = false;
  check(bool, "same bool", given, expected);
};

let test_asubsume_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let he: Hazelnut.Hexp.t = Var("x");
  let ht: Hazelnut.Htyp.t = Num;
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

let test_asubsume_3 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = Var("x");
  let ht: Hazelnut.Htyp.t = Hole;
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

let test_alam_1 = () => {
  let ctx: typctx = TypCtx.empty;
  let he: Hazelnut.Hexp.t = Lam("x", Plus(Lit(1), Lit(2)));
  let ht: Hazelnut.Htyp.t = Arrow(Arrow(Num, Num), Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

let test_alam_2 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Num);
  let he: Hazelnut.Hexp.t = Lam("f", Plus(Var("x"), Lit(2)));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = true;
  check(bool, "same bool", given, expected);
};

let test_alam_3 = () => {
  let ctx: typctx = TypCtx.singleton("x", Hazelnut.Htyp.Arrow(Num, Num));
  let he: Hazelnut.Hexp.t = Lam("f", Plus(Var("x"), Lit(2)));
  let ht: Hazelnut.Htyp.t = Arrow(Num, Num);
  let given: bool = Hazelnut.ana(ctx, he, ht);
  let expected: bool = false;
  check(bool, "same bool", given, expected);
};

let ana_tests = [
  ("test_asubsume_1", `Quick, test_asubsume_1),
  ("test_asubsume_2", `Quick, test_asubsume_2),
  ("test_asubsume_3", `Quick, test_asubsume_3),
  ("test_alam_1", `Quick, test_alam_1),
  ("test_alam_2", `Quick, test_alam_2),
  ("test_alam_3", `Quick, test_alam_3),
];
