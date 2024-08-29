open Core;
open Incr_dom;
open Monad_lib.Monad;
module Hazelnut = Hazelnut_lib.Hazelnut;

// A combination of all Hazelnut types for purposes of printing
module Pmark = {
  type t =
    | InconsistentTypes
    | InconsistentBranches
    | InconsistentAscription
    | Free
    | UnexpectedType(string);
};

module Pexp = {
  type t =
    | Cursor(t)
    | Arrow(t, t)
    | ProductType(t, t)
    | Num
    | Bool
    | Var(string)
    | Lam(string, t, t)
    | Let(string, t, t)
    | Product(t, t)
    | Proj1(t)
    | Proj2(t)
    | Ap(t, t)
    | NumLit(int)
    | BoolLit(bool)
    | Plus(t, t)
    | Cond(t, t, t)
    | Asc(t, t)
    | EHole
    | MarkHole(t, Pmark.t);
};

let pmark_of_mark: Hazelnut.Mark.t => Pmark.t =
  fun
  | InconsistentTypes => InconsistentTypes
  | InconsistentBranches => InconsistentBranches
  | InconsistentAscription => InconsistentAscription
  | Free => Free
  | UnexpectedType(t) =>
    UnexpectedType(Sexp.to_string_hum(Hazelnut.Htyp.sexp_of_t(t)));

let rec pexp_of_htyp: Hazelnut.Htyp.t => Pexp.t =
  fun
  | Arrow(t1, t2) => Arrow(pexp_of_htyp(t1), pexp_of_htyp(t2))
  | Product(t1, t2) => ProductType(pexp_of_htyp(t1), pexp_of_htyp(t2))
  | Num => Num
  | Bool => Bool
  | Hole => EHole;

let rec pexp_of_hexp: Hazelnut.Hexp.t => Pexp.t =
  fun
  | Var(x) => Var(x)
  | Lam(x, a, e) => Lam(x, pexp_of_htyp(a), pexp_of_hexp(e))
  | Let(x, e1, e2) => Let(x, pexp_of_hexp(e1), pexp_of_hexp(e2))
  | Prod(e1, e2) => Product(pexp_of_hexp(e1), pexp_of_hexp(e2))
  | Proj1(e) => Proj1(pexp_of_hexp(e))
  | Proj2(e) => Proj2(pexp_of_hexp(e))
  | Ap(e1, e2) => Ap(pexp_of_hexp(e1), pexp_of_hexp(e2))
  | NumLit(n) => NumLit(n)
  | BoolLit(b) => BoolLit(b)
  | Plus(e1, e2) => Plus(pexp_of_hexp(e1), pexp_of_hexp(e2))
  | Cond(e1, e2, e3) =>
    Cond(pexp_of_hexp(e1), pexp_of_hexp(e2), pexp_of_hexp(e3))
  | Asc(e, t) => Asc(pexp_of_hexp(e), pexp_of_htyp(t))
  | EHole => EHole
  | MarkHole(e, m) => MarkHole(pexp_of_hexp(e), pmark_of_mark(m));

let rec pexp_of_ztyp: Hazelnut.Ztyp.t => Pexp.t =
  fun
  | Cursor(t) => Cursor(pexp_of_htyp(t))
  | LArrow(t1, t2) => Arrow(pexp_of_ztyp(t1), pexp_of_htyp(t2))
  | RArrow(t1, t2) => Arrow(pexp_of_htyp(t1), pexp_of_ztyp(t2))
  | LProd(t1, t2) => ProductType(pexp_of_ztyp(t1), pexp_of_htyp(t2))
  | RProd(t1, t2) => ProductType(pexp_of_htyp(t1), pexp_of_ztyp(t2));

let rec pexp_of_zexp: Hazelnut.Zexp.t => Pexp.t =
  fun
  | Cursor(e) => Cursor(pexp_of_hexp(e))
  | LLam(x, a, e) => Lam(x, pexp_of_ztyp(a), pexp_of_hexp(e))
  | RLam(x, a, e) => Lam(x, pexp_of_htyp(a), pexp_of_zexp(e))
  | LLet(x, e1, e2) => Let(x, pexp_of_zexp(e1), pexp_of_hexp(e2))
  | RLet(x, e1, e2) => Let(x, pexp_of_hexp(e1), pexp_of_zexp(e2))
  | LProd(e1, e2) => Product(pexp_of_zexp(e1), pexp_of_hexp(e2))
  | RProd(e1, e2) => Product(pexp_of_hexp(e1), pexp_of_zexp(e2))
  | Proj1(e) => Proj1(pexp_of_zexp(e))
  | Proj2(e) => Proj2(pexp_of_zexp(e))
  | LAp(e1, e2) => Ap(pexp_of_zexp(e1), pexp_of_hexp(e2))
  | RAp(e1, e2) => Ap(pexp_of_hexp(e1), pexp_of_zexp(e2))
  | LPlus(e1, e2) => Plus(pexp_of_zexp(e1), pexp_of_hexp(e2))
  | RPlus(e1, e2) => Plus(pexp_of_hexp(e1), pexp_of_zexp(e2))
  | IfCond(i, t, e) =>
    Cond(pexp_of_zexp(i), pexp_of_hexp(t), pexp_of_hexp(e))
  | ThenCond(i, t, e) =>
    Cond(pexp_of_hexp(i), pexp_of_zexp(t), pexp_of_hexp(e))
  | ElseCond(i, t, e) =>
    Cond(pexp_of_hexp(i), pexp_of_hexp(t), pexp_of_zexp(e))
  | LAsc(e, t) => Asc(pexp_of_zexp(e), pexp_of_htyp(t))
  | RAsc(e, t) => Asc(pexp_of_hexp(e), pexp_of_ztyp(t))
  | MarkHole(e, m) => MarkHole(pexp_of_zexp(e), pmark_of_mark(m));

// Lower is tighter
let rec prec: Pexp.t => int =
  fun
  | Cursor(e) => prec(e)
  | Arrow(_) => 1
  | Num => 0
  | Bool => 0
  | ProductType(_, _) => 2
  | Var(_) => 0
  | Lam(_) => 0
  | Let(_) => 0
  | Product(_, _) => 0
  | Proj1(_) => 4
  | Proj2(_) => 4
  | Ap(_) => 2
  | NumLit(_) => 0
  | BoolLit(_) => 0
  | Plus(_) => 3
  | Cond(_) => 0
  | Asc(_) => 4
  | EHole => 0
  | MarkHole(_, _) => 0;

module Side = {
  type t =
    | Left
    | Right
    | Atom;
};

let string_of_pmark: Pmark.t => string =
  fun
  | InconsistentTypes => "InconsistentTypes"
  | InconsistentBranches => "InconsistentBranches"
  | InconsistentAscription => "InconsistentAscription"
  | Free => "Free"
  | UnexpectedType(t) => "UnexpectedType(" ++ t ++ ")";

let rec assoc: Pexp.t => Side.t =
  fun
  | Cursor(e) => assoc(e)
  | Arrow(_) => Right
  | Num => Atom
  | Bool => Atom
  | ProductType(_, _) => Atom
  | Var(_) => Atom
  | Lam(_) => Atom
  | Let(_) => Atom
  | Product(_, _) => Atom
  | Proj1(_) => Left
  | Proj2(_) => Left
  | Ap(_) => Left
  | NumLit(_) => Atom
  | BoolLit(_) => Atom
  | Plus(_) => Left
  | Cond(_) => Atom
  | Asc(_) => Left
  | EHole => Atom
  | MarkHole(_, _) => Atom;

let rec string_of_pexp: Pexp.t => string =
  fun
  | Cursor(e) => "👉" ++ string_of_pexp(e) ++ "👈"
  | Arrow(t1, t2) as outer =>
    paren(t1, outer, Side.Left) ++ " -> " ++ paren(t2, outer, Side.Right)
  | Num => "Num"
  | Bool => "Bool"
  | ProductType(t1, t2) as outer =>
    paren(t1, outer, Side.Left) ++ " * " ++ paren(t2, outer, Side.Right)
  | Var(x) => x
  | Lam(x, a, e) =>
    "fun "
    ++ x
    ++ ": "
    ++ string_of_pexp(a)
    ++ " -> {"
    ++ string_of_pexp(e)
    ++ "\n}"
  | Let(x, e1, e2) =>
    "let "
    ++ x
    ++ " = { "
    ++ string_of_pexp(e1)
    ++ " } in { "
    ++ string_of_pexp(e2)
    ++ " }"
  | Product(e1, e2) as outer =>
    paren(e1, outer, Side.Left) ++ ", " ++ paren(e2, outer, Side.Right)
  | Proj1(e) => string_of_pexp(e) ++ ".1"
  | Proj2(e) => string_of_pexp(e) ++ ".2"
  | Ap(e1, e2) as outer =>
    paren(e1, outer, Side.Left) ++ " " ++ paren(e2, outer, Side.Right)
  | NumLit(n) => string_of_int(n)
  | BoolLit(b) => string_of_bool(b)
  | Plus(e1, e2) as outer =>
    paren(e1, outer, Side.Left) ++ " + " ++ paren(e2, outer, Side.Right)
  | Cond(p, e1, e2) as outer =>
    "if "
    ++ paren(p, outer, Side.Left)
    ++ " then { "
    ++ string_of_pexp(e1)
    ++ " } else { "
    ++ string_of_pexp(e2)
    ++ " }"
  | Asc(e, t) as outer =>
    paren(e, outer, Side.Left) ++ ": " ++ paren(t, outer, Side.Right)
  | EHole => "[ ]"
  | MarkHole(e, m) =>
    "[ " ++ string_of_pexp(e) ++ "| " ++ string_of_pmark(m) ++ "| ]"

and paren = (inner: Pexp.t, outer: Pexp.t, side: Side.t): string => {
  let unparenned = string_of_pexp(inner);
  let parenned = "(" ++ unparenned ++ ")";

  let prec_inner = prec(inner);
  let prec_outer = prec(outer);

  if (prec_inner < prec_outer) {
    unparenned;
  } else if (prec_inner > prec_outer) {
    parenned;
  } else {
    switch (assoc(inner), side) {
    | (Side.Left, Side.Right)
    | (Side.Right, Side.Left) => parenned
    | _ => unparenned
    };
  };
};

let check_for_theorem_violation =
    (
      a: Hazelnut.Action.t,
      e: Hazelnut.Zexp.t,
      t: Hazelnut.Htyp.t,
      e': Hazelnut.Zexp.t,
      t': Hazelnut.Htyp.t,
    )
    : option(string) =>
  try({
    let e = Hazelnut.erase_exp(e);
    let e' = Hazelnut.erase_exp(e');

    let theorem_1 = {
      let warning = Some("Theorem 1 violation (Action sensibility)");

      switch (Hazelnut.syn(Hazelnut.TypCtx.empty, e')) {
      | Some(syn_t') =>
        if (Hazelnut.Htyp.compare(t', syn_t') == 0) {
          None;
        } else {
          warning;
        }
      | None => warning
      };
    };

    let theorem_2 =
      switch (a) {
      | Move(_) =>
        if (Hazelnut.Hexp.compare(e, e') == 0
            && Hazelnut.Htyp.compare(t, t') == 0) {
          None;
        } else {
          Some("Theorem 2 violation (movement erasure invariance)");
        }
      | _ => None
      };

    switch (theorem_1, theorem_2) {
    | (Some(_) as warning, _)
    | (_, Some(_) as warning) => warning
    | (None, None) => None
    };
  }) {
  | Hazelnut.Unimplemented => None
  };

[@deriving (sexp, fields, compare)]
type state = {
  e: Hazelnut.Zexp.t,
  t: Hazelnut.Htyp.t,
  warning: option(string),
  var_input: string,
  lam_input: string,
  let_input: string,
  lit_input: string,
  bool_input: string,
};

module Model = {
  [@deriving (sexp, fields, compare)]
  type t = {state};

  let set = (s: state): t => {state: s};

  let init = (): t =>
    set({
      e: Cursor(EHole),
      t: Hole,
      warning: None,
      var_input: "",
      lam_input: "",
      let_input: "",
      lit_input: "",
      bool_input: "true | false",
    });

  let cutoff = (t1: t, t2: t): bool => compare(t1, t2) == 0;
};

module Action = {
  [@deriving sexp]
  type input_location =
    | Var
    | Lam
    | Let
    | NumLit
    | BoolLit;

  [@deriving sexp]
  type action =
    | HazelnutAction(Hazelnut.Action.t)
    | UpdateInput(input_location, string)
    | ShowWarning(string);

  [@deriving sexp]
  type t = list(action);
};

module State = {
  type t = unit;
};

let apply_action =
    (model: Model.t, actions: Action.t, _, ~schedule_action as _): Model.t => {
  let f = (model: Model.t, action: Action.action): Model.t => {
    let state = model.state;

    let warn = (warning: string): Model.t =>
      Model.set({...state, warning: Some(warning)});

    switch (action) {
    | HazelnutAction(action) =>
      try({
        let result =
          Hazelnut.syn_action(
            Hazelnut.TypCtx.empty,
            (state.e, state.t),
            action,
          );

        switch (result) {
        | Some((e, t)) =>
          let new_state = {...state, e, t, warning: None};

          let violation =
            check_for_theorem_violation(action, state.e, state.t, e, t);

          switch (violation) {
          | Some(_) as warning => Model.set({...new_state, warning})
          | None => Model.set(new_state)
          };
        | None => warn("Invalid action")
        };
      }) {
      | Hazelnut.Unimplemented => warn("Unimplemented")
      }
    | UpdateInput(Var, var_input) => Model.set({...state, var_input})
    | UpdateInput(Lam, lam_input) => Model.set({...state, lam_input})
    | UpdateInput(Let, let_input) => Model.set({...state, let_input})
    | UpdateInput(NumLit, lit_input) => Model.set({...state, lit_input})
    | UpdateInput(BoolLit, bool_input) => Model.set({...state, bool_input})
    | ShowWarning(warning) => Model.set({...state, warning: Some(warning)})
    };
  };

  List.fold_left(actions, ~init=model, ~f);
};

let on_startup = (~schedule_action as _, _) => Async_kernel.return();

let view =
    (m: Incr.t(Model.t), ~inject: Action.t => Ui_effect.t(Base.unit))
    : Ui_incr.t(Vdom.Node.t) => {
  open Incr.Let_syntax;
  open Vdom;

  let%map body = {
    let%map state = m >>| Model.state;

    let expression =
      Node.div([
        Node.p([Node.textf("%s", string_of_pexp(pexp_of_zexp(state.e)))]),
        Node.p([Node.textf("%s", string_of_pexp(pexp_of_htyp(state.t)))]),
      ]);

    let buttons = {
      let button =
          (
            label: string,
            action: Action.action,
            input: option((Action.input_location, string)),
          )
          : Node.t => {
        let button_node = {
          let actions =
            switch (input) {
            | Some((input_location, _)) => [
                action,
                Action.UpdateInput(input_location, ""),
              ]
            | None => [action]
            };

          Node.button(
            ~attr=
              Attr.many_without_merge([
                Attr.on_click(_ev => inject(actions)),
              ]),
            [Node.text(label)],
          );
        };

        let input_node = {
          let+ (input_location, input_value) = input;
          Node.input(
            ~attr=
              Attr.many_without_merge([
                Attr.type_("text"),
                Attr.string_property("value", input_value),
                Attr.on_input((_ev, text) =>
                  inject([Action.UpdateInput(input_location, text)])
                ),
              ]),
            [],
          );
        };

        Node.div(
          switch (input_node) {
          | Some(input_node) => [button_node, input_node]
          | None => [button_node]
          },
        );
      };

      let move_buttons =
        Node.div([
          button(
            "Move to Parent",
            Action.HazelnutAction(Move(Parent)),
            None,
          ),
          button(
            "Move to Child 1",
            Action.HazelnutAction(Move(Child(One))),
            None,
          ),
          button(
            "Move to Child 2",
            Action.HazelnutAction(Move(Child(Two))),
            None,
          ),
          button(
            "Move to Child 3",
            Action.HazelnutAction(Move(Child(Three))),
            None,
          ),
        ]);

      let construct_buttons =
        Node.div([
          button(
            "Construct Arrow",
            Action.HazelnutAction(Construct(Arrow)),
            None,
          ),
          button(
            "Construct Num",
            Action.HazelnutAction(Construct(Num)),
            None,
          ),
          button(
            "Construct Bool",
            Action.HazelnutAction(Construct(Bool)),
            None,
          ),
          button(
            "Construct Asc",
            Action.HazelnutAction(Construct(Asc)),
            None,
          ),
          button(
            "Construct Var",
            Action.HazelnutAction(Construct(Var(state.var_input))),
            Some((Var, state.var_input)),
          ),
          button(
            "Construct Lam",
            Action.HazelnutAction(Construct(Lam(state.lam_input))),
            Some((Lam, state.lam_input)),
          ),
          button(
            "Construct Let",
            Action.HazelnutAction(Construct(Let(state.let_input))),
            Some((Let, state.let_input)),
          ),
          button(
            "Construct Ap",
            Action.HazelnutAction(Construct(Ap)),
            None,
          ),
          button(
            "Construct NumLit",
            try(
              Action.HazelnutAction(
                Construct(NumLit(int_of_string(state.lit_input))),
              )
            ) {
            | Failure(_) => Action.ShowWarning("Invalid input")
            },
            Some((NumLit, state.lit_input)),
          ),
          button(
            "Construct Bool",
            switch (bool_of_string_opt(state.bool_input)) {
            | Some(b) => Action.HazelnutAction(Construct(BoolLit(b)))
            | None => Action.ShowWarning("Invalid input")
            },
            Some((BoolLit, state.bool_input)),
          ),
          button(
            "Construct Plus",
            Action.HazelnutAction(Construct(Plus)),
            None,
          ),
          button(
            "Construct Cond",
            Action.HazelnutAction(Construct(Cond)),
            None,
          ),
          button(
            "Construct NEHole",
            Action.HazelnutAction(Construct(NEHole)),
            None,
          ),
        ]);

      let delete_button =
        Node.div([button("Delete", Action.HazelnutAction(Del), None)]);

      let finish_button =
        Node.div([button("Finish", Action.HazelnutAction(Finish), None)]);

      Node.div([
        move_buttons,
        construct_buttons,
        delete_button,
        finish_button,
      ]);
    };

    let warning =
      Node.p(
        switch (state.warning) {
        | Some(warning) => [Node.text(warning)]
        | None => []
        },
      );

    Node.div([expression, buttons, warning]);
  };

  Node.body([body]);
};

let create = (model, ~old_model as _, ~inject) => {
  open Incr.Let_syntax;
  let%map apply_action = {
    let%map model = model;
    apply_action(model);
  }
  and view = view(model, ~inject)
  and model = model;
  Component.create(~apply_action, model, view);
};

let initial_model = Model.init();
