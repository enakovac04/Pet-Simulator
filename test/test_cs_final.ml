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
        (let animal = create "Darien" "Dog" in 
        decrease_health animal 2; get_health animal)
          8);

        ("Testing Pet.decrease_health under min" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Dog" in 
        decrease_health animal 12; get_health animal)
          0);

        ("Testing Pet.increase_health" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Dog" in 
        decrease_health animal 2; increase_health animal 1; get_health animal)
          9);

        ("Testing Pet.increase_health" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Dog" in 
        increase_health animal 1; get_health animal)
          10);

        ("Testing Pet.decrease_health" >:: fun _ ->
          assert_equal 8 (let animal = create "Darien" "Dog" in 
          decrease_health animal 2; get_health animal));
        ("Testing Pet.decrease_health under minimum" >:: fun _ ->
          assert_equal 0 (let animal = create "Darien" "Dog" in 
          decrease_health animal 12; get_health animal));
        ("Testing Pet.increase_health within bounds" >:: fun _ ->
          assert_equal 9 (let animal = create "Darien" "Dog" in 
          decrease_health animal 2; increase_health animal 1; get_health animal));
        ("Testing Pet.increase_health to maximum" >:: fun _ ->
          assert_equal 10 (let animal = create "Darien" "Dog" in 
          increase_health animal 1; get_health animal));

        ("Testing Pet.increase_happiness to maximum" >:: fun _ ->
          assert_equal 10 (let animal = create "Darien" "Dog" in 
          increase_happiness animal 5; get_happiness animal));
        ("Testing Pet.decrease_happiness to zero" >:: fun _ ->
          assert_equal 0 (let animal = create "Darien" "Dog" in 
          decrease_happiness animal 15; get_happiness animal));

        ("Testing Pet.increase_energy to maximum" >:: fun _ ->
          assert_equal 10 (let animal = create "Darien" "Dog" in 
          increase_energy animal 5; get_energy animal));
        ("Testing Pet.decrease_energy to zero" >:: fun _ ->
          assert_equal 0 (let animal = create "Darien" "Dog" in 
          decrease_energy animal 15; get_energy animal));

        ("Testing Pet.set_health" >:: fun _ ->
          let animal = create "Darien" "Dog" in 
          set_health animal 7; assert_equal 7 (get_health animal));
        ("Testing Pet.set_happiness" >:: fun _ ->
          let animal = create "Darien" "Dog" in 
          set_happiness animal 8; assert_equal 8 (get_happiness animal));
        ("Testing Pet.set_energy" >:: fun _ ->
          let animal = create "Darien" "Dog" in 
          set_energy animal 6; assert_equal 6 (get_energy animal));
        ("Testing Pet.set_nutrition" >:: fun _ ->
          let animal = create "Darien" "Dog" in 
          set_nutrition animal 4; assert_equal 4 (get_nutrition animal));

        ("Comprehensive Dog Attributes Test" >:: fun _ ->
          let dog = create "Sparky" "Dog" in
          decrease_health dog 3;
          increase_happiness dog 2;
          decrease_energy dog 4;
          set_nutrition dog 8; 
          assert_equal "Dog Sparky has 7/10 health, 10/10 happiness, 6/10 energy, 8/10 nutrition, and $5." (status_to_string dog)
        );

        ("Stress Test on Camel Attributes" >:: fun _ ->
          let camel = create "Sandy" "Camel" in
          increase_health camel 20;
          decrease_happiness camel 20;
          increase_energy camel 15;
          assert_equal "Camel Sandy has 10/10 health, 0/10 happiness, 10/10 energy, 10/10 nutrition, and $5." (status_to_string camel)
        );

        ("Boundary Conditions on Dog Health" >:: fun _ ->
          let dog = create "Bolt" "Dog" in
          decrease_health dog 10;
          increase_health dog 1;  
          assert_equal 1 (get_health dog);
          decrease_health dog 1; 
          assert_equal 0 (get_health dog);
        );

        ("Exact Setting Test for Camel" >:: fun _ ->
          let camel = create "Carmen" "Camel" in
          set_health camel 5;
          set_happiness camel 5;
          set_energy camel 5;
          set_nutrition camel 5;
          assert_equal "Camel Carmen has 5/10 health, 5/10 happiness, 5/10 energy, 5/10 nutrition, and $5." (status_to_string camel)
        );

        ("Reset and Modify Dog" >:: fun _ ->
          let dog = create "Rover" "Dog" in
          increase_happiness dog 5;
          decrease_happiness dog 3;  
          increase_happiness dog 3; 
          decrease_health dog 3;     
          set_health dog 10; 
          assert_equal "Dog Rover has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5." (status_to_string dog)
        );

        ("Extreme Modification for Camel" >:: fun _ ->
          let camel = create "Dusty" "Camel" in
          increase_health camel 5;
          decrease_happiness camel 15;
          increase_energy camel 10;
          set_nutrition camel 0; 
          assert_equal "Camel Dusty has 10/10 health, 0/10 happiness, 10/10 energy, 0/10 nutrition, and $5." (status_to_string camel)
        );
      ]

let () = run_test_tt_main tests