open OUnit;;
open Commondefs;;
open Level;;

let v0 = { id=0; coord_x=490; coord_y=200; };;
let v1 = { id=1; coord_x=204; coord_y=35; };;
let v2 = { id=2; coord_x=395; coord_y=35; };;
let v3 = { id=3; coord_x=110; coord_y=200; };;
let v4 = { id=4; coord_x=205; coord_y=364; };;
let v5 = { id=5; coord_x=395; coord_y=364; };;

let testintersect sl1 sl2 =
  match (cross sl1 sl2) with
    Intersection(x, y) -> ()
  | No_Intersection -> assert_failure (Printf.sprintf "lines %s and %s should cross" (string_of_line sl1.l) (string_of_line sl2.l));;

let crosstest_hor_lr = TestCase(fun _ ->
  let sl1 = create_slopedline(create_line v4 v2) in
  let sl2 = create_slopedline(create_line v3 v0) in
  testintersect sl1 sl2);;

let crosstest_hor_lr_rev = TestCase(fun _ ->
  let sl1 = create_slopedline(create_line v4 v2) in
  let sl2 = create_slopedline(create_line v0 v3) in
  testintersect sl1 sl2);;

let crosstest_hor_rl = TestCase(fun _ ->
  let sl1 = create_slopedline(create_line v4 v2) in
  let sl2 = create_slopedline(create_line v5 v1) in
  testintersect sl1 sl2);;

let crosstest_hor_ver = TestCase(fun _ ->
  let sl1 = create_slopedline(create_line v4 v2) in
  let sl2 = create_slopedline(create_line v5 v1) in
  testintersect sl1 sl2);;
  
let suite = TestLabel ("suite", 
  TestList [TestLabel ("crosstest_hor_lr", crosstest_hor_lr);
            TestLabel ("crosstest_hor_lr_rev", crosstest_hor_lr_rev);
            TestLabel ("crosstest_hor_rl", crosstest_hor_rl);
            TestLabel ("crosstest_hor_ver", crosstest_hor_ver);]);;


let main = run_test_tt suite;;

  
