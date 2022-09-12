open Core;
open Incr_dom;
module Hazelnut = Hazelnut_lib.Hazelnut;

// Maybe Monad
let ( let* ) = (x: option('a), f: 'a => option('b)): option('b) =>
  switch (x) {
  | Some(x) => f(x)
  | None => None
  };

// Maybe Monad
let (let+) = (x: option('a), f: 'a => 'b): option('b) => {
  let* x = x;
  Some(f(x));
};

let string_of_cursor = (e: string): string => "👉" ++ e ++ "👈";
let string_of_arrow = (t1: string, t2: string): string =>
  "(" ++ t1 ++ ") -> (" ++ t2 ++ ")";
let string_of_lam = (x: string, e: string): string =>
  "fun " ++ x ++ " -> { " ++ e ++ " }";
let string_of_ap = (e1: string, e2: string): string =>
  "(" ++ e1 ++ ") (" ++ e2 ++ ")";
let string_of_plus = (e1: string, e2: string): string =>
  "(" ++ e1 ++ ") + (" ++ e2 ++ ")";
let string_of_asc = (e: string, t: string): string =>
  "(" ++ e ++ "): (" ++ t ++ ")";
let string_of_ehole: string = "[ ]";
let string_of_nehole = (e: string): string => "[ " ++ e ++ " ]";

let rec string_of_htyp: Hazelnut.htyp => string =
  fun
  | Arrow(t1, t2) =>
    string_of_arrow(string_of_htyp(t1), string_of_htyp(t2))
  | Num => "Num"
  | Hole => string_of_ehole;

let rec string_of_hexp: Hazelnut.hexp => string =
  fun
  | Var(x) => x
  | Lam(x, e) => string_of_lam(x, string_of_hexp(e))
  | Ap(e1, e2) => string_of_ap(string_of_hexp(e1), string_of_hexp(e2))
  | Num(n) => string_of_int(n)
  | Plus(e1, e2) => string_of_plus(string_of_hexp(e1), string_of_hexp(e2))
  | Asc(e, t) => string_of_asc(string_of_hexp(e), string_of_htyp(t))
  | EHole => string_of_ehole
  | NEHole(e) => string_of_nehole(string_of_hexp(e));

let rec string_of_ztyp: Hazelnut.ztyp => string =
  fun
  | Cursor(t) => string_of_cursor(string_of_htyp(t))
  | LArrow(t1, t2) =>
    string_of_arrow(string_of_ztyp(t1), string_of_htyp(t2))
  | RArrow(t1, t2) =>
    string_of_arrow(string_of_htyp(t1), string_of_ztyp(t2));

let rec string_of_zexp: Hazelnut.zexp => string =
  fun
  | Cursor(e) => string_of_cursor(string_of_hexp(e))
  | Lam(x, e) => string_of_lam(x, string_of_zexp(e))
  | LAp(e1, e2) => string_of_ap(string_of_zexp(e1), string_of_hexp(e2))
  | RAp(e1, e2) => string_of_ap(string_of_hexp(e1), string_of_zexp(e2))
  | LPlus(e1, e2) => string_of_plus(string_of_zexp(e1), string_of_hexp(e2))
  | RPlus(e1, e2) => string_of_plus(string_of_hexp(e1), string_of_zexp(e2))
  | LAsc(e, t) => string_of_asc(string_of_zexp(e), string_of_htyp(t))
  | RAsc(e, t) => string_of_asc(string_of_hexp(e), string_of_ztyp(t))
  | NEHole(e) => string_of_nehole(string_of_zexp(e));

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
        | Some((e, t)) => Model.set({...state, e, t, warning: None})
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
        Node.p([Node.textf("%s", string_of_zexp(state.e))]),
        Node.p([Node.textf("%s", string_of_htyp(state.t))]),
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
            Action.HazelnutAction(Construct(Var(state.var_input))), // TODO: Don't hardcode value
            Some((Var, state.var_input)),
          ),
          button(
            "Construct Lam",
            Action.HazelnutAction(Construct(Lam(state.lam_input))), // TODO: Don't hardcode value
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
