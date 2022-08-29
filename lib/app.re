open Core;
open Incr_dom;
module Hazelnut = Hazelnut_lib.Hazelnut;

let render_zexp: Hazelnut.zexp => string =
  fun
  | Cursor(_) => "Cursor"
  | Lam(_, _) => "Lam"
  | LAp(_, _) => "LAp"
  | RAp(_, _) => "RAp"
  | LPlus(_, _) => "LPlus"
  | RPlus(_, _) => "RPlus"
  | LAsc(_, _) => "LAsc"
  | RAsc(_, _) => "RAsc"
  | NEHole(_) => "NEHole";

module Model = {
  [@deriving (sexp, fields, compare)]
  type t = {e: Hazelnut.zexp};

  let set_default_expression = (e: Hazelnut.zexp): t => {e: e};

  let init = (): t => set_default_expression(Cursor(EHole));
  let do_nothing = (e: t): t => e;
  let cutoff = (t1: t, t2: t): bool => compare(t1, t2) == 0;
};

module Action = {
  [@deriving sexp]
  type t =
    | DoNothing;
};

module State = {
  type t = unit;
};

let apply_action = (model, action, _, ~schedule_action as _) =>
  switch ((action: Action.t)) {
  | DoNothing => Model.do_nothing(model)
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
    let do_nothing_button = button("Do Nothing", Action.DoNothing);

    Node.div([do_nothing_button]);
  };

  let%map counter = {
    let%map e = m >>| Model.e;
    let e = render_zexp(e);
    Node.div([Node.textf("%s", e)]);
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
