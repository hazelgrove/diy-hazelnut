open Alcotest;

let () =
  run(
    "Hazelnut_tests",
    [
      ("erase_exp", Test_erase_exp.erase_exp_tests),
      ("syn", Test_syn.syn_tests),
      ("ana", Test_ana.ana_tests),
      ("ana_action", Test_ana_action.ana_action_tests),
      ("syn_action", Test_syn_action.syn_action_tests),
      ("sample 1", Test_sample1.sample1_tests),
      ("sample 2", Test_sample2.sample2_tests),
      ("type_action", Test_type_action.type_action_tests),
    ],
  );
