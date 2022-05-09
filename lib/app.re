open Core;
open Incr_dom;

module Model = {
  [@deriving (sexp, fields, compare)]
  type t = {counter: int};

  let set_default_input = (counter: int): t => {counter: counter};

  let init = (): t => set_default_input(0);
  let reset_counter = (_: t): t => set_default_input(0);
  let incr_counter = (t: t): t => set_default_input(t.counter + 1);
  let decr_counter = (t: t): t => set_default_input(t.counter - 1);
  let cutoff = (t1: t, t2: t): bool => compare(t1, t2) == 0;
};

module Action = {
  [@deriving sexp]
  type t =
    | Reset_counter
    | Incr_counter
    | Decr_counter;
};

module State = {
  type t = unit;
};

let apply_action = (model, action, _, ~schedule_action as _) =>
  switch ((action: Action.t)) {
  | Reset_counter => Model.reset_counter(model)
  | Incr_counter => Model.incr_counter(model)
  | Decr_counter => Model.decr_counter(model)
  };

let on_startup = (~schedule_action as _, _) => Async_kernel.return();

let view =
    (m: Incr.t(Model.t), ~inject: Action.t => Ui_effect.t(Base.unit))
    : Ui_incr.t(Vdom.Node.t) => {
  open Incr.Let_syntax;
  open Vdom;

  let button = (label, action) =>
    Node.button(
      ~attr=
        Attr.many_without_merge([
          Attr.id(String.lowercase(label)),
          Attr.on_click(_ev => inject(action)),
        ]),
      [Node.text(label)],
    );

  let buttons = {
    let reset_button = button("Reset", Action.Reset_counter);
    let incr_button = button("+", Action.Incr_counter);
    let decr_button = button("-", Action.Decr_counter);

    Node.div([decr_button, reset_button, incr_button]);
  };

  let%map counter = {
    let%map counter = m >>| Model.counter;
    Node.div([Node.textf("%d", counter)]);
  };

  Node.body([counter, buttons]);
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
