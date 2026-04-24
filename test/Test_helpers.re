open Alcotest;
module Hazelnut = Hazelnut_lib.Hazelnut;

// =====================================================================
// Tests for helper functions: consistent, inconsistent, matched_arrow,
// erase_typ. These correspond to Definitions 1-3 and Appendix A.2.1.
// =====================================================================

// --- Type consistency (Definition 1) ---

let test_consistent_num_num = () =>
  check(bool, "num ~ num", Hazelnut.consistent(Num, Num), true);

let test_consistent_hole_num = () =>
  check(bool, "hole ~ num", Hazelnut.consistent(Hole, Num), true);

let test_consistent_num_hole = () =>
  check(bool, "num ~ hole", Hazelnut.consistent(Num, Hole), true);

let test_consistent_hole_hole = () =>
  check(bool, "hole ~ hole", Hazelnut.consistent(Hole, Hole), true);

let test_consistent_arrow_arrow = () =>
  check(
    bool,
    "(num->num) ~ (num->num)",
    Hazelnut.consistent(Arrow(Num, Num), Arrow(Num, Num)),
    true,
  );

let test_consistent_arrow_hole_domain = () =>
  check(
    bool,
    "(hole->num) ~ (num->num)",
    Hazelnut.consistent(Arrow(Hole, Num), Arrow(Num, Num)),
    true,
  );

let test_consistent_arrow_hole_codomain = () =>
  check(
    bool,
    "(num->hole) ~ (num->num)",
    Hazelnut.consistent(Arrow(Num, Hole), Arrow(Num, Num)),
    true,
  );

let test_consistent_hole_arrow = () =>
  check(
    bool,
    "hole ~ (num->num)",
    Hazelnut.consistent(Hole, Arrow(Num, Num)),
    true,
  );

let test_consistent_nested_arrows = () =>
  check(
    bool,
    "((hole->num)->num) ~ ((num->num)->num)",
    Hazelnut.consistent(
      Arrow(Arrow(Hole, Num), Num),
      Arrow(Arrow(Num, Num), Num),
    ),
    true,
  );

// --- Type inconsistency (Definition 2) ---

let test_inconsistent_num_arrow = () =>
  check(
    bool,
    "num /~ (num->num)",
    Hazelnut.inconsistent(Num, Arrow(Num, Num)),
    true,
  );

let test_inconsistent_arrow_num = () =>
  check(
    bool,
    "(num->num) /~ num",
    Hazelnut.inconsistent(Arrow(Num, Num), Num),
    true,
  );

let test_inconsistent_arrow_mismatch = () =>
  check(
    bool,
    "(num->num) /~ (num->(num->num))",
    Hazelnut.inconsistent(Arrow(Num, Num), Arrow(Num, Arrow(Num, Num))),
    true,
  );

let test_not_inconsistent_num_num = () =>
  check(bool, "num consistent with num", Hazelnut.inconsistent(Num, Num), false);

let test_not_inconsistent_hole = () =>
  check(
    bool,
    "hole consistent with anything",
    Hazelnut.inconsistent(Hole, Arrow(Num, Num)),
    false,
  );

// --- Matched arrow types (Definition 3) ---

let ma_eq =
    (
      a: option((Hazelnut.Htyp.t, Hazelnut.Htyp.t)),
      b: option((Hazelnut.Htyp.t, Hazelnut.Htyp.t)),
    )
    : bool =>
  switch (a, b) {
  | (Some((t1, t2)), Some((t1', t2'))) =>
    Hazelnut.Htyp.compare(t1, t1') == 0
    && Hazelnut.Htyp.compare(t2, t2') == 0
  | (None, None) => true
  | _ => false
  };

let ma_print = (v: option((Hazelnut.Htyp.t, Hazelnut.Htyp.t))): string =>
  switch (v) {
  | Some((t1, t2)) =>
    "Some((" ++ Hazelnut.Htyp.show(t1) ++ ", " ++ Hazelnut.Htyp.show(t2) ++ "))"
  | None => "None"
  };

let ma_typ = testable(Fmt.using(ma_print, Fmt.string), ma_eq);

let test_matched_arrow_arrow = () =>
  check(
    ma_typ,
    "Arrow(Num, Hole) => Some(Num, Hole)",
    Hazelnut.matched_arrow(Arrow(Num, Hole)),
    Some((Num, Hole)),
  );

let test_matched_arrow_hole = () =>
  check(
    ma_typ,
    "Hole => Some(Hole, Hole)",
    Hazelnut.matched_arrow(Hole),
    Some((Hole, Hole)),
  );

let test_matched_arrow_num = () =>
  check(
    ma_typ,
    "Num => None",
    Hazelnut.matched_arrow(Num),
    None,
  );

let test_matched_arrow_nested = () =>
  check(
    ma_typ,
    "Arrow(Arrow(Num, Num), Hole) => Some(Arrow(Num,Num), Hole)",
    Hazelnut.matched_arrow(Arrow(Arrow(Num, Num), Hole)),
    Some((Arrow(Num, Num), Hole)),
  );

// --- Type cursor erasure (Appendix A.2.1) ---

let htyp_eq = (a: Hazelnut.Htyp.t, b: Hazelnut.Htyp.t): bool =>
  Hazelnut.Htyp.compare(a, b) == 0;

let htyp_typ =
  testable(Fmt.using(Hazelnut.Htyp.show, Fmt.string), htyp_eq);

let test_erase_typ_cursor = () =>
  check(
    htyp_typ,
    "erase(Cursor(Num)) = Num",
    Hazelnut.erase_typ(Cursor(Num)),
    Hazelnut.Htyp.Num,
  );

let test_erase_typ_larrow = () =>
  check(
    htyp_typ,
    "erase(LArrow(Cursor(Num), Hole)) = Arrow(Num, Hole)",
    Hazelnut.erase_typ(LArrow(Cursor(Num), Hole)),
    Hazelnut.Htyp.Arrow(Num, Hole),
  );

let test_erase_typ_rarrow = () =>
  check(
    htyp_typ,
    "erase(RArrow(Num, Cursor(Hole))) = Arrow(Num, Hole)",
    Hazelnut.erase_typ(RArrow(Num, Cursor(Hole))),
    Hazelnut.Htyp.Arrow(Num, Hole),
  );

let test_erase_typ_nested = () =>
  check(
    htyp_typ,
    "erase nested LArrow in RArrow",
    Hazelnut.erase_typ(RArrow(Num, LArrow(Cursor(Num), Hole))),
    Hazelnut.Htyp.Arrow(Num, Arrow(Num, Hole)),
  );

let test_erase_typ_deep = () =>
  check(
    htyp_typ,
    "erase deeply nested cursor",
    Hazelnut.erase_typ(LArrow(LArrow(Cursor(Hole), Num), Num)),
    Hazelnut.Htyp.Arrow(Arrow(Hole, Num), Num),
  );

let helpers_tests = [
  // consistent
  ("consistent_num_num", `Quick, test_consistent_num_num),
  ("consistent_hole_num", `Quick, test_consistent_hole_num),
  ("consistent_num_hole", `Quick, test_consistent_num_hole),
  ("consistent_hole_hole", `Quick, test_consistent_hole_hole),
  ("consistent_arrow_arrow", `Quick, test_consistent_arrow_arrow),
  ("consistent_arrow_hole_domain", `Quick, test_consistent_arrow_hole_domain),
  ("consistent_arrow_hole_codomain", `Quick, test_consistent_arrow_hole_codomain),
  ("consistent_hole_arrow", `Quick, test_consistent_hole_arrow),
  ("consistent_nested_arrows", `Quick, test_consistent_nested_arrows),
  // inconsistent
  ("inconsistent_num_arrow", `Quick, test_inconsistent_num_arrow),
  ("inconsistent_arrow_num", `Quick, test_inconsistent_arrow_num),
  ("inconsistent_arrow_mismatch", `Quick, test_inconsistent_arrow_mismatch),
  ("not_inconsistent_num_num", `Quick, test_not_inconsistent_num_num),
  ("not_inconsistent_hole", `Quick, test_not_inconsistent_hole),
  // matched_arrow
  ("matched_arrow_arrow", `Quick, test_matched_arrow_arrow),
  ("matched_arrow_hole", `Quick, test_matched_arrow_hole),
  ("matched_arrow_num", `Quick, test_matched_arrow_num),
  ("matched_arrow_nested", `Quick, test_matched_arrow_nested),
  // erase_typ
  ("erase_typ_cursor", `Quick, test_erase_typ_cursor),
  ("erase_typ_larrow", `Quick, test_erase_typ_larrow),
  ("erase_typ_rarrow", `Quick, test_erase_typ_rarrow),
  ("erase_typ_nested", `Quick, test_erase_typ_nested),
  ("erase_typ_deep", `Quick, test_erase_typ_deep),
];
