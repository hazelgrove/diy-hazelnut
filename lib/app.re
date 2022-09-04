open Core;
open Incr_dom;
module Hazelnut = Hazelnut_lib.Hazelnut;

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

module Model = {
  [@deriving (sexp, fields, compare)]
  type t = {e: Hazelnut.zexp};

  let set_default_expression = (e: Hazelnut.zexp): t => {e: e};

  let init = (): t =>
    set_default_expression(Cursor(Plus(Num(1), Num(2))));
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

  let%map expression = {
    let%map e = m >>| Model.e;
    let expression_text = string_of_zexp(e);
    Node.div([Node.textf("%s", expression_text)]);
  };

  Node.body([expression, buttons]);
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
