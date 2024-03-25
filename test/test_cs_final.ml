open OUnit2
open Cs_final
open Pet

let test_pet name (input : animal) expected_output =
  name >:: fun _ -> assert_equal expected_output (to_string input)

let tests =
  " Testing Myset functions with Txtfileset"
  >::: [
         test_pet "Testing Pet.create" (create "Claire" "Camel")
           "Camel Claire has 10/10 health and $5.";
         test_pet "Testing Pet.create" (create "Darien" "Dog")
           "Dog Darien has 10/10 health and $5.";
       ]

let () = run_test_tt_main tests
