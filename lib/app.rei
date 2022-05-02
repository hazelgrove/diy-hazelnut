open! Core;

module Model: {
  [@deriving sexp]
  type t;

  let cutoff: (t, t) => bool;
};

include Incr_dom.App_intf.S with type State.t = unit and module Model := Model;

let initial_model: Model.t;
