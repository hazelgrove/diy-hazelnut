open Core;
open Incr_dom;

module Model = {
  [@deriving (sexp, fields, compare)]
  type t = {
    counter: int,
    input_text: string,
    submitted_text: option(string),
  };

  let set_default_input = (counter, submitted_text) => {
    counter,
    input_text: sprintf("Default #%d", counter),
    submitted_text,
  };

  let init = () => set_default_input(0, None);
  let reset_counter = t => set_default_input(0, t.submitted_text);
  let incr_counter = t => set_default_input(t.counter + 1, t.submitted_text);
  let update_input = (t, input_text) => {...t, input_text};
  let submit_input = t => {...t, submitted_text: Some(t.input_text)};
  let cutoff = (t1, t2) => compare(t1, t2) == 0;
};

module Action = {
  [@deriving sexp]
  type t =
    | Reset_counter
    | Incr_counter
    | Update_input(string)
    | Submit_input;
};

module State = {
  type t = unit;
};

let apply_action = (model, action, _, ~schedule_action as _) =>
  switch ((action: Action.t)) {
  | Reset_counter => Model.reset_counter(model)
  | Incr_counter => Model.incr_counter(model)
  | Update_input(text) => Model.update_input(model, text)
  | Submit_input => Model.submit_input(model)
  };

let on_startup = (~schedule_action as _, _) => Async_kernel.return();

let view = (m: Incr.t(Model.t), ~inject) => {
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

  let submit_button = button("Submit", Action.Submit_input);
  let reset_button = button("Reset", Action.Reset_counter);
  let incr_button = button("Increment", Action.Incr_counter);
  let%map input = {
    let%map input_text = m >>| Model.input_text;
    Node.input(
      ~attr=
        Attr.many_without_merge([
          Attr.id("input"),
          Attr.type_("text"),
          /* The value property controls the current value of the text input, whereas the
             value attribute only controls its initial value. */
          Attr.string_property("value", input_text),
          /* We must update our model with the user's input to keep the virtual dom consistent
             with the actual dom. */
          Attr.on_input((_ev, text) => inject(Action.Update_input(text))),
        ]),
      [],
    );
  }
  and submission = {
    let%map submitted_text = m >>| Model.submitted_text;
    let text =
      switch (submitted_text) {
      | None => "No submissions yet"
      | Some(text) => "Your latest submission was: " ++ text
      };

    Node.div([Node.text(text)]);
  };

  Node.body([submission, input, submit_button, reset_button, incr_button]);
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
