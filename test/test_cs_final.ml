open OUnit2
open Cs_final
open Pet

let test_pet name (input : animal) func expected_output =
  name >:: fun _ -> assert_equal expected_output (func input)
let tests =
  " Testing Myset functions with Txtfileset"
  >::: [
        test_pet "Testing Pet.create" (create "Claire" "Camel") status_to_string
          "Camel Claire has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5.";
        test_pet "Testing Pet.create" (create "Darien" "Dog") status_to_string
          "Dog Darien has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5.";
        test_pet "Testing Pet.get_name" (create "Darien" "Dog") get_name
          "Darien";
        test_pet "Testing Pet.health_to_string" (create "Darien" "Dog") health_to_string
        "Dog Darien has 10/10 health";
        test_pet "Testing Pet.happiness_to_string" (create "Darien" "Dog") happiness_to_string
        "Dog Darien has 10/10 happiness";
        test_pet "Testing Pet.energy_to_string" (create "Darien" "Dog") energy_to_string
        "Dog Darien has 10/10 energy";
        test_pet "Testing Pet.nutrition_to_string" (create "Darien" "Dog") nutrition_to_string
        "Dog Darien has 10/10 nutrition";
        test_pet "Testing Pet.money_to_string" (create "Darien" "Dog") money_to_string
        "Dog Darien has $5.";

        ("Testing Pet.decrease_health" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Dog" in decrease_health animal 2; get_health animal)
          8);
        ("Testing Pet.decrease_health under min" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Dog" in decrease_health animal 12; get_health animal)
          0);

        ("Testing Pet.increase_health" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Dog" in decrease_health animal 2; increase_health animal 1; get_health animal)
          9);
        ("Testing Pet.increase_health" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Dog" in increase_health animal 1; get_health animal)
          10);
       ]

let () = run_test_tt_main tests
