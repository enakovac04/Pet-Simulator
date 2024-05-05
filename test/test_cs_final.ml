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
        test_pet "Testing Pet.get_name2" (create "Darien" "Camel") get_name
          "Darien";
        test_pet "Testing Pet.health_to_string2" (create "Darien" "Camel") health_to_string
        "Camel Darien has 10/10 health";
        test_pet "Testing Pet.happiness_to_string2" (create "Darien" "Camel") happiness_to_string
        "Camel Darien has 10/10 happiness";
        test_pet "Testing Pet.energy_to_string2" (create "Darien" "Camel") energy_to_string
        "Camel Darien has 10/10 energy";
        test_pet "Testing Pet.nutrition_to_string2" (create "Darien" "Camel") nutrition_to_string
        "Camel Darien has 10/10 nutrition";
        test_pet "Testing Pet.money_to_string2" (create "Darien" "Camel") money_to_string
        "Camel Darien has $5.";

        ("Full attribute set and reset for Camel1" >:: fun _ ->
          let camel = create "Humphrey" "Camel" in
          set_health camel 6;
          set_happiness camel 8;
          set_energy camel 5;
          set_nutrition camel 3;
          assert_equal "Camel Humphrey has 6/10 health, 8/10 happiness, 5/10 energy, 3/10 nutrition, and $5." (status_to_string camel);
          set_health camel 10;
          set_happiness camel 10;
          set_energy camel 10;
          set_nutrition camel 10;
          assert_equal "Camel Humphrey has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5." (status_to_string camel)
        );

        ("Full attribute set and reset for Dog1" >:: fun _ ->
          let camel = create "Humphrey" "Dog" in
          set_health camel 7;
          set_happiness camel 8;
          set_energy camel 5;
          set_nutrition camel 3;
          assert_equal "Dog Humphrey has 7/10 health, 8/10 happiness, 5/10 energy, 3/10 nutrition, and $5." (status_to_string camel);
          set_health camel 10;
          set_happiness camel 10;
          set_energy camel 10;
          set_nutrition camel 10;
          assert_equal "Dog Humphrey has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5." (status_to_string camel)
        ); 

        ("Testing Pet.decrease_health" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Dog" in 
        decrease_health animal 2; get_health animal)
          8);

        ("Testing Pet.decrease_health2" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Camel" in 
        decrease_health animal 2; get_health animal)
          8);

        ("Testing Pet.decrease_health under min" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Dog" in 
        decrease_health animal 12; get_health animal)
          0);

        ("Testing Pet.decrease_health under min2" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Camel" in 
        decrease_health animal 12; get_health animal)
          0);

        ("Testing Pet.increase_health" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Dog" in 
        decrease_health animal 2; increase_health animal 1; get_health animal)
          9);

        ("Testing Pet.increase_health2" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Camel" in 
        decrease_health animal 2; increase_health animal 1; get_health animal)
          9);
        
        ("Testing Pet.increase_health3" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Camel" in 
        decrease_health animal 2; increase_health animal 4; get_health animal)
          10);

        ("Testing Pet.increase_health" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Dog" in 
        increase_health animal 1; get_health animal)
          10);
        
        ("Testing Pet.increase_health2" >:: fun _ -> assert_equal 
        (let animal = create "Darien" "Camel" in 
        increase_health animal 1; get_health animal)
          10);

        ("Testing Pet.decrease_health" >:: fun _ ->
          assert_equal 8 (let animal = create "Darien" "Dog" in 
          decrease_health animal 2; get_health animal)
          );

        ("Testing Pet.decrease_health2" >:: fun _ ->
          assert_equal 8 (let animal = create "Darien" "Camel" in 
          decrease_health animal 2; get_health animal)
          );

        ("Testing Pet.decrease_health under minimum" >:: fun _ ->
          assert_equal 0 (let animal = create "Darien" "Dog" in 
          decrease_health animal 12; get_health animal)
          );

         ("Testing Pet.decrease_health under minimum2" >:: fun _ ->
          assert_equal 0 (let animal = create "Darien" "Camel" in 
          decrease_health animal 12; get_health animal)
          );

        ("Testing Pet.increase_health within bounds" >:: fun _ ->
          assert_equal 9 (let animal = create "Darien" "Dog" in 
          decrease_health animal 2; increase_health animal 1; get_health animal)
          );

        ("Testing Pet.increase_health within bounds2" >:: fun _ ->
          assert_equal 9 (let animal = create "Darien" "Camel" in 
          decrease_health animal 2; increase_health animal 1; get_health animal)
          );

        ("Testing Pet.increase_health to maximum" >:: fun _ ->
          assert_equal 10 (let animal = create "Darien" "Dog" in 
          increase_health animal 1; get_health animal)
          );

        ("Testing Pet.increase_health to maximum2" >:: fun _ ->
          assert_equal 10 (let animal = create "Darien" "Camel" in 
          increase_health animal 1; get_health animal)
          );

        ("Testing Pet.increase_happiness to maximum" >:: fun _ ->
          assert_equal 10 (let animal = create "Darien" "Dog" in 
          increase_happiness animal 5; get_happiness animal)
          );

        ("Testing Pet.increase_happiness to maximum2" >:: fun _ ->
          assert_equal 10 (let animal = create "Darien" "Camel" in 
          increase_happiness animal 5; get_happiness animal)
          );

        ("Testing Pet.decrease_happiness to zero" >:: fun _ ->
          assert_equal 0 (let animal = create "Darien" "Dog" in 
          decrease_happiness animal 15; get_happiness animal)
          );

          ("Testing Pet.decrease_happiness to zero2" >:: fun _ ->
            assert_equal 0 (let animal = create "Darien" "Camel" in 
            decrease_happiness animal 15; get_happiness animal)
            );

        ("Testing Pet.increase_energy to maximum" >:: fun _ ->
          assert_equal 10 (let animal = create "Darien" "Dog" in 
          increase_energy animal 5; get_energy animal)
          );

        ("Testing Pet.increase_energy to maximum2" >:: fun _ ->
          assert_equal 10 (let animal = create "Darien" "Camel" in 
          increase_energy animal 5; get_energy animal)
          );

        ("Testing Pet.decrease_energy to zero" >:: fun _ ->
          assert_equal 0 (let animal = create "Darien" "Dog" in 
          decrease_energy animal 15; get_energy animal)
          );

        ("Testing Pet.decrease_energy to zero2" >:: fun _ ->
          assert_equal 0 (let animal = create "Darien" "Camel" in 
          decrease_energy animal 15; get_energy animal)
          );

        ("Testing Pet.set_health" >:: fun _ ->
          let animal = create "Darien" "Dog" in 
          set_health animal 7; assert_equal 7 (get_health animal)
          );

        ("Testing Pet.set_health2" >:: fun _ ->
          let animal = create "Darien" "Camel" in 
          set_health animal 7; assert_equal 7 (get_health animal)
          );

        ("Testing Pet.set_happiness" >:: fun _ ->
          let animal = create "Darien" "Dog" in 
          set_happiness animal 8; assert_equal 8 (get_happiness animal)
          );

        ("Testing Pet.set_happiness2" >:: fun _ ->
          let animal = create "Darien" "Camel" in 
          set_happiness animal 8; assert_equal 8 (get_happiness animal)
          );

        ("Testing Pet.set_energy" >:: fun _ ->
          let animal = create "Darien" "Dog" in 
          set_energy animal 6; assert_equal 6 (get_energy animal)
          );

        ("Testing Pet.set_energy2" >:: fun _ ->
          let animal = create "Darien" "Camel" in 
          set_energy animal 6; assert_equal 6 (get_energy animal)
          );

        ("Testing Pet.set_nutrition" >:: fun _ ->
          let animal = create "Darien" "Dog" in 
          set_nutrition animal 4; assert_equal 4 (get_nutrition animal)
          );

        ("Testing Pet.set_nutrition2" >:: fun _ ->
          let animal = create "Darien" "Camel" in 
          set_nutrition animal 4; assert_equal 4 (get_nutrition animal)
          );

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

        ("Reset and Modify Camel" >:: fun _ ->
          let dog = create "Rover" "Camel" in
          increase_happiness dog 5;
          decrease_happiness dog 3;  
          increase_happiness dog 3; 
          decrease_health dog 3;     
          set_health dog 10; 
          assert_equal "Camel Rover has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5." (status_to_string dog)
        );

        ("Extreme Modification for Camel" >:: fun _ ->
          let camel = create "Dusty" "Camel" in
          increase_health camel 5;
          decrease_happiness camel 15;
          increase_energy camel 10;
          set_nutrition camel 0; 
          assert_equal "Camel Dusty has 10/10 health, 0/10 happiness, 10/10 energy, 0/10 nutrition, and $5." (status_to_string camel)
        );

        ("Extreme Modification for Dog" >:: fun _ ->
          let camel = create "Dusty" "Dog" in
          increase_health camel 5;
          decrease_happiness camel 15;
          increase_energy camel 10;
          set_nutrition camel 0; 
          assert_equal "Dog Dusty has 10/10 health, 0/10 happiness, 10/10 energy, 0/10 nutrition, and $5." (status_to_string camel)
        );

        ("Decrease Dog health to zero and check negative boundary" >:: fun _ ->
          let dog = create "Sam" "Dog" in
          decrease_health dog 15; 
          assert_equal 0 (get_health dog)
        );

        ("Increase Camel happiness beyond max" >:: fun _ ->
          let camel = create "Sandy" "Camel" in
          increase_happiness camel 20;
          assert_equal 10 (get_happiness camel)
        );

        ("Decrease Camel happiness to zero" >:: fun _ ->
          let camel = create "Sandy" "Camel" in
          decrease_happiness camel 15;
          assert_equal 0 (get_happiness camel)
        );

        ("Decrease Dog energy to zero and test lower limit" >:: fun _ ->
          let dog = create "Rocky" "Dog" in
          decrease_energy dog 12; 
          assert_equal 0 (get_energy dog)
        );

        ("Increase Camel energy to test upper limit" >:: fun _ ->
          let camel = create "Joe" "Camel" in
          increase_energy camel 5;
          assert_equal 10 (get_energy camel)
        );

        ("Reset Camel nutrition and modify" >:: fun _ ->
          let camel = create "Molly" "Camel" in
          set_nutrition camel 5;
          increase_nutrition camel 3;
          decrease_nutrition camel 1;
          assert_equal 7 (get_nutrition camel)
        );

        ("Extreme nutrition adjustment for Dog" >:: fun _ ->
          let dog = create "Buddy" "Dog" in
          set_nutrition dog 2;
          increase_nutrition dog 20;
          assert_equal 10 (get_nutrition dog)
        );

        ("Extreme nutrition adjustment for Camel" >:: fun _ ->
          let dog = create "Buddy" "Camel" in
          set_nutrition dog 2;
          increase_nutrition dog 20;
          assert_equal 10 (get_nutrition dog)
        );

        ("Full attribute set and reset for Camel" >:: fun _ ->
          let camel = create "Humphrey" "Camel" in
          set_health camel 7;
          set_happiness camel 8;
          set_energy camel 5;
          set_nutrition camel 3;
          assert_equal "Camel Humphrey has 7/10 health, 8/10 happiness, 5/10 energy, 3/10 nutrition, and $5." (status_to_string camel);
          set_health camel 10;
          set_happiness camel 10;
          set_energy camel 10;
          set_nutrition camel 10;
          assert_equal "Camel Humphrey has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5." (status_to_string camel)
        );

        ("Full attribute set and reset again for Camel" >:: fun _ ->
          let camel = create "Humphrey" "Camel" in
          set_health camel 6;
          set_happiness camel 8;
          set_energy camel 5;
          set_nutrition camel 3;
          assert_equal "Camel Humphrey has 6/10 health, 8/10 happiness, 5/10 energy, 3/10 nutrition, and $5." (status_to_string camel);
          set_health camel 10;
          set_happiness camel 10;
          set_energy camel 10;
          set_nutrition camel 10;
          assert_equal "Camel Humphrey has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5." (status_to_string camel)
        );

        ("Full attribute set and reset for Dog" >:: fun _ ->
          let camel = create "Humphrey" "Dog" in
          set_health camel 7;
          set_happiness camel 8;
          set_energy camel 5;
          set_nutrition camel 3;
          assert_equal "Dog Humphrey has 7/10 health, 8/10 happiness, 5/10 energy, 3/10 nutrition, and $5." (status_to_string camel);
          set_health camel 10;
          set_happiness camel 10;
          set_energy camel 10;
          set_nutrition camel 10;
          assert_equal "Dog Humphrey has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5." (status_to_string camel)
        ); 

        ("Full attribute adjustment for Camel" >:: fun _ ->
          let camel = create "Cleo" "Camel" in
          set_health camel 6;
          set_happiness camel 4;
          set_energy camel 8;
          set_nutrition camel 7;
          assert_equal "Camel Cleo has 6/10 health, 4/10 happiness, 8/10 energy, 7/10 nutrition, and $5." (status_to_string camel);
          set_health camel 10;
          set_happiness camel 10;
          set_energy camel 10;
          set_nutrition camel 10;
          assert_equal "Camel Cleo has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5." (status_to_string camel)
        );
        
        ("Attribute reset for Dog after extreme conditions" >:: fun _ ->
          let dog = create "Barker" "Dog" in
          decrease_health dog 9;
          increase_happiness dog 0;
          decrease_energy dog 5; 
          increase_nutrition dog 2;
          assert_equal "Dog Barker has 1/10 health, 10/10 happiness, 5/10 energy, 10/10 nutrition, and $5." (status_to_string dog);
          set_health dog 10;
          set_happiness dog 10;
          set_energy dog 10;
          set_nutrition dog 10;
          assert_equal "Dog Barker has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5." (status_to_string dog)
        );
        
        ("Comprehensive attribute modification for Dog 'Rusty'" >:: fun _ ->
          let dog = create "Rusty" "Dog" in
          set_health dog 8;
          set_happiness dog 6;
          set_energy dog 3;
          set_nutrition dog 5;
          assert_equal "Dog Rusty has 8/10 health, 6/10 happiness, 3/10 energy, 5/10 nutrition, and $5." (status_to_string dog);
          set_health dog 10;
          set_happiness dog 10;
          set_energy dog 10;
          set_nutrition dog 10;
          assert_equal "Dog Rusty has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5." (status_to_string dog)
        );
        
        ("Adjusting and checking full range of Camel attributes" >:: fun _ ->
          let camel = create "Spike" "Camel" in
          set_health camel 2;
          set_happiness camel 1;
          set_energy camel 4;
          set_nutrition camel 9;
          assert_equal "Camel Spike has 2/10 health, 1/10 happiness, 4/10 energy, 9/10 nutrition, and $5." (status_to_string camel);
          set_health camel 10;
          set_happiness camel 10;
          set_energy camel 10;
          set_nutrition camel 10;
          assert_equal "Camel Spike has 10/10 health, 10/10 happiness, 10/10 energy, 10/10 nutrition, and $5." (status_to_string camel)
        );
      ]

let () = run_test_tt_main tests