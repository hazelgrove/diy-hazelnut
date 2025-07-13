open! Core;

module Model: {
  [@deriving sexp]
  type t;

  let cutoff: (t, t) => bool;
};
module Hazelnut = Hazelnut_lib.Hazelnut;

include Incr_dom.App_intf.S with type State.t = unit and module Model := Model;

let initial_model: Model.t;
let fold_zexp_mexp: (Hazelnut.Zexp.t, Hazelnut.Hexp.t) => Hazelnut.Zexp.t;
