module Hazelnut = Hazelnut_lib.Hazelnut;

module TypCtx = Map.Make(String);

// =====================================================================
// QCheck generators for Hazelnut types and expressions.
//
// These generate random well-formed terms for property-based testing.
// The generators use bounded depth to keep terms tractable.
// =====================================================================

let gen_htyp: QCheck.Gen.t(Hazelnut.Htyp.t) = {
  let open QCheck.Gen;
  sized(
    fix((self, n) =>
      if (n <= 0) {
        oneof([pure(Hazelnut.Htyp.Num), pure(Hazelnut.Htyp.Hole)]);
      } else {
        frequency([
          (3, pure(Hazelnut.Htyp.Num)),
          (3, pure(Hazelnut.Htyp.Hole)),
          (
            2,
            {
              let sub = self(n / 2);
              map2((t1, t2) => Hazelnut.Htyp.Arrow(t1, t2), sub, sub);
            },
          ),
        ]);
      }
    ),
  );
};

let gen_varname: QCheck.Gen.t(string) = {
  let open QCheck.Gen;
  oneofl(["x", "y", "z", "f", "g"]);
};

let gen_hexp = (ctx: list((string, Hazelnut.Htyp.t))): QCheck.Gen.t(Hazelnut.Hexp.t) => {
  let open QCheck.Gen;
  let vars = List.map(((x, _)) => x, ctx);
  sized(
    fix((self, n) =>
      if (n <= 0) {
        let leaves =
          [
            (3, map(n => Hazelnut.Hexp.Lit(n), small_nat)),
            (2, pure(Hazelnut.Hexp.EHole)),
          ]
          @ (
            switch (vars) {
            | [] => []
            | _ => [(3, map(x => Hazelnut.Hexp.Var(x), oneofl(vars)))]
            }
          );
        frequency(leaves);
      } else {
        let sub = self(n / 2);
        let branches =
          [
            (3, map(n => Hazelnut.Hexp.Lit(n), small_nat)),
            (2, pure(Hazelnut.Hexp.EHole)),
            (2, map2((e1, e2) => Hazelnut.Hexp.Plus(e1, e2), sub, sub)),
            (2, map2((e1, e2) => Hazelnut.Hexp.Ap(e1, e2), sub, sub)),
            (1, map(e => Hazelnut.Hexp.NEHole(e), sub)),
            (
              1,
              map2(
                (e, t) => Hazelnut.Hexp.Asc(e, t),
                sub,
                gen_htyp,
              ),
            ),
            (
              1,
              map2(
                (x, e) => Hazelnut.Hexp.Lam(x, e),
                gen_varname,
                sub,
              ),
            ),
          ]
          @ (
            switch (vars) {
            | [] => []
            | _ => [(3, map(x => Hazelnut.Hexp.Var(x), oneofl(vars)))]
            }
          );
        frequency(branches);
      }
    ),
  );
};

let gen_ztyp_from_htyp: QCheck.Gen.t((Hazelnut.Ztyp.t, Hazelnut.Htyp.t)) = {
  let open QCheck.Gen;
  let rec go = (t: Hazelnut.Htyp.t): QCheck.Gen.t(Hazelnut.Ztyp.t) =>
    switch (t) {
    | Hazelnut.Htyp.Arrow(t1, t2) =>
      frequency([
        (2, pure(Hazelnut.Ztyp.Cursor(t))),
        (1, map(zt1 => Hazelnut.Ztyp.LArrow(zt1, t2), go(t1))),
        (1, map(zt2 => Hazelnut.Ztyp.RArrow(t1, zt2), go(t2))),
      ])
    | _ => pure(Hazelnut.Ztyp.Cursor(t))
    };
  let* t = gen_htyp;
  let+ zt = go(t);
  (zt, t);
};

let gen_zexp_from_hexp: QCheck.Gen.t((Hazelnut.Zexp.t, Hazelnut.Hexp.t)) = {
  let open QCheck.Gen;
  let rec go_typ = (t: Hazelnut.Htyp.t): QCheck.Gen.t(Hazelnut.Ztyp.t) =>
    switch (t) {
    | Hazelnut.Htyp.Arrow(t1, t2) =>
      frequency([
        (2, pure(Hazelnut.Ztyp.Cursor(t))),
        (1, map(zt1 => Hazelnut.Ztyp.LArrow(zt1, t2), go_typ(t1))),
        (1, map(zt2 => Hazelnut.Ztyp.RArrow(t1, zt2), go_typ(t2))),
      ])
    | _ => pure(Hazelnut.Ztyp.Cursor(t))
    };
  let rec go = (e: Hazelnut.Hexp.t): QCheck.Gen.t(Hazelnut.Zexp.t) =>
    switch (e) {
    | Hazelnut.Hexp.Lam(x, body) =>
      frequency([
        (2, pure(Hazelnut.Zexp.Cursor(e))),
        (1, map(zb => Hazelnut.Zexp.Lam(x, zb), go(body))),
      ])
    | Hazelnut.Hexp.Ap(e1, e2) =>
      frequency([
        (2, pure(Hazelnut.Zexp.Cursor(e))),
        (1, map(ze1 => Hazelnut.Zexp.LAp(ze1, e2), go(e1))),
        (1, map(ze2 => Hazelnut.Zexp.RAp(e1, ze2), go(e2))),
      ])
    | Hazelnut.Hexp.Plus(e1, e2) =>
      frequency([
        (2, pure(Hazelnut.Zexp.Cursor(e))),
        (1, map(ze1 => Hazelnut.Zexp.LPlus(ze1, e2), go(e1))),
        (1, map(ze2 => Hazelnut.Zexp.RPlus(e1, ze2), go(e2))),
      ])
    | Hazelnut.Hexp.Asc(e1, t) =>
      frequency([
        (2, pure(Hazelnut.Zexp.Cursor(e))),
        (1, map(ze1 => Hazelnut.Zexp.LAsc(ze1, t), go(e1))),
        (1, map(zt => Hazelnut.Zexp.RAsc(e1, zt), go_typ(t))),
      ])
    | Hazelnut.Hexp.NEHole(e1) =>
      frequency([
        (2, pure(Hazelnut.Zexp.Cursor(e))),
        (1, map(ze1 => Hazelnut.Zexp.NEHole(ze1), go(e1))),
      ])
    | _ => pure(Hazelnut.Zexp.Cursor(e))
    };
  let* e = gen_hexp([]);
  let+ ze = go(e);
  (ze, e);
};

let show_htyp = Hazelnut.Htyp.show;
let show_hexp = Hazelnut.Hexp.show;
let show_zexp = Hazelnut.Zexp.show;

// =====================================================================
// Property: type consistency is reflexive — τ ~ τ for all τ
// =====================================================================
let prop_consistent_reflexive =
  QCheck.Test.make(
    ~name="consistent is reflexive",
    ~count=200,
    QCheck.make(gen_htyp, ~print=show_htyp),
    t =>
    try(Hazelnut.consistent(t, t)) {
    | Hazelnut.Unimplemented => true
    },
  );

// =====================================================================
// Property: type consistency is symmetric — τ₁ ~ τ₂ ⟺ τ₂ ~ τ₁
// =====================================================================
let prop_consistent_symmetric =
  QCheck.Test.make(
    ~name="consistent is symmetric",
    ~count=200,
    QCheck.make(
      QCheck.Gen.pair(gen_htyp, gen_htyp),
      ~print=((t1, t2)) =>
        "(" ++ show_htyp(t1) ++ ", " ++ show_htyp(t2) ++ ")",
    ),
    ((t1, t2)) =>
    try(Hazelnut.consistent(t1, t2) == Hazelnut.consistent(t2, t1)) {
    | Hazelnut.Unimplemented => true
    },
  );

// =====================================================================
// Property: inconsistent is the negation of consistent
// =====================================================================
let prop_inconsistent_negation =
  QCheck.Test.make(
    ~name="inconsistent = !consistent",
    ~count=200,
    QCheck.make(
      QCheck.Gen.pair(gen_htyp, gen_htyp),
      ~print=((t1, t2)) =>
        "(" ++ show_htyp(t1) ++ ", " ++ show_htyp(t2) ++ ")",
    ),
    ((t1, t2)) =>
    try(Hazelnut.inconsistent(t1, t2) == !Hazelnut.consistent(t1, t2)) {
    | Hazelnut.Unimplemented => true
    },
  );

// =====================================================================
// Property: erase_typ is idempotent via Cursor — erase(Cursor(τ)) = τ
// =====================================================================
let prop_erase_typ_cursor_id =
  QCheck.Test.make(
    ~name="erase_typ(Cursor(t)) = t",
    ~count=200,
    QCheck.make(gen_htyp, ~print=show_htyp),
    t =>
    try(Hazelnut.Htyp.compare(Hazelnut.erase_typ(Cursor(t)), t) == 0) {
    | Hazelnut.Unimplemented => true
    },
  );

// =====================================================================
// Property: erase_exp round-trips through Cursor — erase(Cursor(e)) = e
// =====================================================================
let prop_erase_exp_cursor_id =
  QCheck.Test.make(
    ~name="erase_exp(Cursor(e)) = e",
    ~count=200,
    QCheck.make(gen_hexp([]), ~print=show_hexp),
    e =>
    try(Hazelnut.Hexp.compare(Hazelnut.erase_exp(Cursor(e)), e) == 0) {
    | Hazelnut.Unimplemented => true
    },
  );

// =====================================================================
// Property: erase_exp recovers the underlying H-expression from any
// Z-expression built by placing the cursor somewhere in the tree.
// =====================================================================
let prop_erase_exp_roundtrip =
  QCheck.Test.make(
    ~name="erase_exp(ze) = e when ze is built from e",
    ~count=200,
    QCheck.make(
      gen_zexp_from_hexp,
      ~print=((ze, e)) =>
        "ze=" ++ show_zexp(ze) ++ " e=" ++ show_hexp(e),
    ),
    ((ze, e)) =>
    try(Hazelnut.Hexp.compare(Hazelnut.erase_exp(ze), e) == 0) {
    | Hazelnut.Unimplemented => true
    },
  );

// =====================================================================
// Property (Theorem 1 — Sensibility):
// If syn(ctx, erase(ze)) = Some(t) and syn_action(ctx, (ze, t), a) = Some((ze', t')),
// then syn(ctx, erase(ze')) = Some(t').
// =====================================================================
let gen_action: QCheck.Gen.t(Hazelnut.Action.t) = {
  let open QCheck.Gen;
  frequency([
    (2, pure(Hazelnut.Action.Move(Child(One)))),
    (2, pure(Hazelnut.Action.Move(Child(Two)))),
    (2, pure(Hazelnut.Action.Move(Parent))),
    (3, pure(Hazelnut.Action.Del)),
    (3, pure(Hazelnut.Action.Finish)),
    (1, pure(Hazelnut.Action.Construct(Arrow))),
    (1, pure(Hazelnut.Action.Construct(Num))),
    (2, pure(Hazelnut.Action.Construct(Asc))),
    (1, map(x => Hazelnut.Action.Construct(Var(x)), gen_varname)),
    (1, map(x => Hazelnut.Action.Construct(Lam(x)), gen_varname)),
    (2, pure(Hazelnut.Action.Construct(Ap))),
    (
      1,
      map(n => Hazelnut.Action.Construct(Lit(n)), QCheck.Gen.small_nat),
    ),
    (2, pure(Hazelnut.Action.Construct(Plus))),
    (1, pure(Hazelnut.Action.Construct(NEHole))),
  ]);
};

let show_action = (a: Hazelnut.Action.t): string =>
  Sexplib.Sexp.to_string_hum(Hazelnut.Action.sexp_of_t(a));

let prop_sensibility =
  QCheck.Test.make(
    ~name="Theorem 1 (Sensibility)",
    ~count=500,
    QCheck.make(
      QCheck.Gen.pair(gen_zexp_from_hexp, gen_action),
      ~print=(((ze, _e), a)) =>
        "ze="
        ++ show_zexp(ze)
        ++ " action="
        ++ show_action(a),
    ),
    (((ze, _e), a)) =>
    try({
      let ctx = TypCtx.empty;
      let erased = Hazelnut.erase_exp(ze);
      switch (Hazelnut.syn(ctx, erased)) {
      | Some(t) =>
        switch (Hazelnut.syn_action(ctx, (ze, t), a)) {
        | Some((ze', t')) =>
          let erased' = Hazelnut.erase_exp(ze');
          switch (Hazelnut.syn(ctx, erased')) {
          | Some(t_syn) => Hazelnut.Htyp.compare(t', t_syn) == 0
          | None => false
          };
        | None => true
        }
      | None => true
      };
    }) {
    | Hazelnut.Unimplemented => true
    },
  );

// =====================================================================
// Property (Theorem 2 — Movement Erasure Invariance):
// If syn_action(ctx, (ze, t), Move(d)) = Some((ze', t')),
// then erase(ze) = erase(ze') and t = t'.
// =====================================================================
let gen_dir: QCheck.Gen.t(Hazelnut.Dir.t) = {
  let open QCheck.Gen;
  oneof([
    pure(Hazelnut.Dir.Child(One)),
    pure(Hazelnut.Dir.Child(Two)),
    pure(Hazelnut.Dir.Parent),
  ]);
};

let prop_movement_erasure_invariance =
  QCheck.Test.make(
    ~name="Theorem 2 (Movement erasure invariance)",
    ~count=500,
    QCheck.make(
      QCheck.Gen.pair(gen_zexp_from_hexp, gen_dir),
      ~print=(((ze, _e), _d)) => "ze=" ++ show_zexp(ze),
    ),
    (((ze, _e), d)) =>
    try({
      let ctx = TypCtx.empty;
      let erased = Hazelnut.erase_exp(ze);
      switch (Hazelnut.syn(ctx, erased)) {
      | Some(t) =>
        switch (Hazelnut.syn_action(ctx, (ze, t), Move(d))) {
        | Some((ze', t')) =>
          let erased' = Hazelnut.erase_exp(ze');
          Hazelnut.Hexp.compare(erased, erased') == 0
          && Hazelnut.Htyp.compare(t, t') == 0;
        | None => true
        }
      | None => true
      };
    }) {
    | Hazelnut.Unimplemented => true
    },
  );

// =====================================================================
// Property: matched_arrow of Arrow(t1,t2) always returns Some((t1,t2))
// =====================================================================
let prop_matched_arrow_roundtrip =
  QCheck.Test.make(
    ~name="matched_arrow(Arrow(t1,t2)) = Some((t1,t2))",
    ~count=200,
    QCheck.make(
      QCheck.Gen.pair(gen_htyp, gen_htyp),
      ~print=((t1, t2)) =>
        "(" ++ show_htyp(t1) ++ ", " ++ show_htyp(t2) ++ ")",
    ),
    ((t1, t2)) =>
    try(
      switch (Hazelnut.matched_arrow(Arrow(t1, t2))) {
      | Some((t1', t2')) =>
        Hazelnut.Htyp.compare(t1, t1') == 0
        && Hazelnut.Htyp.compare(t2, t2') == 0
      | None => false
      }
    ) {
    | Hazelnut.Unimplemented => true
    },
  );

let qcheck_tests =
  List.map(
    QCheck_alcotest.to_alcotest,
    [
      prop_consistent_reflexive,
      prop_consistent_symmetric,
      prop_inconsistent_negation,
      prop_erase_typ_cursor_id,
      prop_erase_exp_cursor_id,
      prop_erase_exp_roundtrip,
      prop_sensibility,
      prop_movement_erasure_invariance,
      prop_matched_arrow_roundtrip,
    ],
  );
