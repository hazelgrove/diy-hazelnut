open Alcotest;

let () =
  run(
    "Hazelnut_tests",
    [
      ("erase_exp", Test_erase_exp.erase_exp_tests),
      ("syn", Test_syn.syn_tests),
      ("ana", Test_ana.ana_tests),
      ("syn_action", Test_syn_action.syn_action_tests),
      ("ana_action", Test_ana_action.ana_action_tests),
    ],
  );
