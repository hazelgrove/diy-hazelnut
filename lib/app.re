open Core;
open Incr_dom;
open Monad_lib.Monad;
module Hazelnut = Hazelnut_lib.Hazelnut;

// A combination of all Hazelnut types for purposes of printing
type pexp =
  | Cursor(pexp)
  | Arrow(pexp, pexp)
  | Num
  | Var(string)
  | Lam(string, pexp)
  | Ap(pexp, pexp)
  | Lit(int)
  | Plus(pexp, pexp)
  | Asc(pexp, pexp)
  | EHole
  | NEHole(pexp);

let rec pexp_of_htyp: Hazelnut.htyp => pexp =
  fun
  | Arrow(t1, t2) => Arrow(pexp_of_htyp(t1), pexp_of_htyp(t2))
  | Num => Num
  | Hole => EHole;

let rec pexp_of_hexp: Hazelnut.hexp => pexp =
  fun
  | Var(x) => Var(x)
  | Lam(x, e) => Lam(x, pexp_of_hexp(e))
  | Ap(e1, e2) => Ap(pexp_of_hexp(e1), pexp_of_hexp(e2))
  | Lit(n) => Lit(n)
  | Plus(e1, e2) => Plus(pexp_of_hexp(e1), pexp_of_hexp(e2))
  | Asc(e, t) => Asc(pexp_of_hexp(e), pexp_of_htyp(t))
  | EHole => EHole
  | NEHole(e) => NEHole(pexp_of_hexp(e));

let rec pexp_of_ztyp: Hazelnut.ztyp => pexp =
  fun
  | Cursor(t) => Cursor(pexp_of_htyp(t))
  | LArrow(t1, t2) => Arrow(pexp_of_ztyp(t1), pexp_of_htyp(t2))
  | RArrow(t1, t2) => Arrow(pexp_of_htyp(t1), pexp_of_ztyp(t2));

let rec pexp_of_zexp: Hazelnut.zexp => pexp =
  fun
  | Cursor(e) => Cursor(pexp_of_hexp(e))
  | Lam(x, e) => Lam(x, pexp_of_zexp(e))
  | LAp(e1, e2) => Ap(pexp_of_zexp(e1), pexp_of_hexp(e2))
  | RAp(e1, e2) => Ap(pexp_of_hexp(e1), pexp_of_zexp(e2))
  | LPlus(e1, e2) => Plus(pexp_of_zexp(e1), pexp_of_hexp(e2))
  | RPlus(e1, e2) => Plus(pexp_of_hexp(e1), pexp_of_zexp(e2))
  | LAsc(e, t) => Asc(pexp_of_zexp(e), pexp_of_htyp(t))
  | RAsc(e, t) => Asc(pexp_of_hexp(e), pexp_of_ztyp(t))
  | NEHole(e) => NEHole(pexp_of_zexp(e));

// Lower is tighter
let rec prec: pexp => int =
  fun
  | Cursor(e) => prec(e)
  | Arrow(_) => 1
  | Num => 0
  | Var(_) => 0
  | Lam(_) => 0
  | Ap(_) => 2
  | Lit(_) => 0
  | Plus(_) => 3
  | Asc(_) => 4
  | EHole => 0
  | NEHole(_) => 0;

type side =
  | Left
  | Right
  | Atom;

let rec assoc: pexp => side =
  fun
  | Cursor(e) => assoc(e)
  | Arrow(_) => Right
  | Num => Atom
  | Var(_) => Atom
  | Lam(_) => Atom
  | Ap(_) => Left
  | Lit(_) => Atom
  | Plus(_) => Left
  | Asc(_) => Left
  | EHole => Atom
  | NEHole(_) => Atom;

let rec string_of_pexp: pexp => string =
  fun
  | Cursor(e) => "👉" ++ string_of_pexp(e) ++ "👈"
  | Arrow(t1, t2) as outer =>
    paren(t1, outer, Left) ++ " -> " ++ paren(t2, outer, Right)
  | Num => "Num"
  | Var(x) => x
  | Lam(x, e) => "fun " ++ x ++ " -> { " ++ string_of_pexp(e) ++ " }"
  | Ap(e1, e2) as outer =>
    paren(e1, outer, Left) ++ " " ++ paren(e2, outer, Right)
  | Lit(n) => string_of_int(n)
  | Plus(e1, e2) as outer =>
    paren(e1, outer, Left) ++ " + " ++ paren(e2, outer, Right)
  | Asc(e, t) as outer =>
    paren(e, outer, Left) ++ ": " ++ paren(t, outer, Right)
  | EHole => "[ ]"
  | NEHole(e) => "[ " ++ string_of_pexp(e) ++ " ]"

and paren = (inner: pexp, outer: pexp, side: side): string => {
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
    | (Left, Right)
    | (Right, Left) => parenned
    | _ => unparenned
    };
  };
};

let check_for_theorem_violation =
    (
      a: Hazelnut.action,
      e: Hazelnut.zexp,
      t: Hazelnut.htyp,
      e': Hazelnut.zexp,
      t': Hazelnut.htyp,
    )
    : option(string) =>
  try({
    let e = Hazelnut.erase_exp(e);
    let e' = Hazelnut.erase_exp(e');

    let theorem_1 = {
      let warning = Some("Theorem 1 violation (Action sensibility)");

      switch (Hazelnut.syn(Hazelnut.TypCtx.empty, e')) {
      | Some(syn_t') =>
        if (Hazelnut.compare_htyp(t', syn_t') == 0) {
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
        if (Hazelnut.compare_hexp(e, e') == 0
            && Hazelnut.compare_htyp(t, t') == 0) {
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
  e: Hazelnut.zexp,
  t: Hazelnut.htyp,
  warning: option(string),
  var_input: string,
  lam_input: string,
  lit_input: string,
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
      lit_input: "",
    });

  let cutoff = (t1: t, t2: t): bool => compare(t1, t2) == 0;
};

module Action = {
  [@deriving sexp]
  type input_location =
    | Var
    | Lam
    | Lit;

  [@deriving sexp]
  type action =
    | HazelnutAction(Hazelnut.action)
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
    | UpdateInput(Lit, lit_input) => Model.set({...state, lit_input})
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
            "Construct Ap",
            Action.HazelnutAction(Construct(Ap)),
            None,
          ),
          button(
            "Construct Lit",
            try(
              Action.HazelnutAction(
                Construct(Lit(int_of_string(state.lit_input))),
              )
            ) {
            | Failure(_) => Action.ShowWarning("Invalid input")
            },
            Some((Lit, state.lit_input)),
          ),
          button(
            "Construct Plus",
            Action.HazelnutAction(Construct(Plus)),
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
