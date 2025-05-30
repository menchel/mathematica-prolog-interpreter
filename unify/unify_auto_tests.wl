varX = "X";
varY = "Y";
varZ = "Z";
constA = "a";
constB = "b";
constC = "c";

placeholder = "_";

list1 = {"a", "b", "X"};
list2 = {"Y", "b", "c"};
list3 = {"a", "b", "c"};
emptyList = {};

comp1 = <|"Compound" -> "f", "Arguments" -> {varX, constA}|>;
comp2 = <|"Compound" -> "f", "Arguments" -> {constB, varY}|>;
comp3 = <|"Compound" -> "g", "Arguments" -> {varX, constA}|>;
comp4 = <|"Compound" -> "h", "Arguments" -> {varX, varY, varZ}|>;
comp5 = <|"Compound" -> "h", "Arguments" -> {constA, constB, constC}|>;

pred1 = <|"head" -> "p", "arguments" -> {varX, constA}|>;
pred2 = <|"head" -> "p", "arguments" -> {constB, varY}|>;
pred3 = <|"head" -> "q", "arguments" -> {varX, placeholder}|>;

listStruct1 = <|"ListHead" -> "a", "Tail" -> <|"ListHead" -> "b", "Tail" -> <|"ListHead" -> "X", "Tail" -> "nil"|>|>|>;
listStruct2 = <|"ListHead" -> "Y", "Tail" -> <|"ListHead" -> "b", "Tail" -> <|"ListHead" -> "c", "Tail" -> "nil"|>|>|>;

printTest[name_, actual_, expected_] := 
  Print[Row[{name, ": ", 
    If[actual === expected, 
     Style["\[Checkmark] PASS: " <> ToString[actual], Darker@Green], 
     Style["✗ FAIL: " <> ToString[actual] <> " (expected: " <> ToString[expected] <> ")", Red]]}]];

Print["\nBASIC TESTS:"];
printTest["unify[varX, constA]", unify[varX, constA], <|"X" -> "a"|>];
printTest["unify[varX, varY]", unify[varX, varY], <|"X" -> "Y"|>];
printTest["unify[constA, constB]", unify[constA, constB], $Failed];
printTest["unify[constA, constA]", unify[constA, constA], <||>];

Print["\nPLACEHOLDER TESTS:"];
printTest["unify[placeholder, constA]", unify[placeholder, constA], <||>];
printTest["unify[varX, placeholder]", unify[varX, placeholder], <||>];
printTest["unify[placeholder, placeholder]", unify[placeholder, placeholder], <||>];
printTest["unify[pred1, pred3]", unify[pred1, pred3], $Failed];

Print["\nLIST TESTS:"];
printTest["unify[list1, list2]", unify[list1, list2], <|"Y" -> "a","X" -> "c"|>];
printTest["unify[list1, list3]", unify[list1, list3], <|"X" -> "c"|>];
printTest["unify[emptyList, emptyList]", unify[emptyList, emptyList], <||>];
printTest["unify[list1, emptyList]", unify[list1, emptyList], $Failed];

Print["\nCOMPOUND TERM TESTS:"];
printTest["unify[comp1, comp2]", unify[comp1, comp2], <|"X" -> "b", "Y" -> "a"|>];
printTest["unify[comp1, comp3]", unify[comp1, comp3], $Failed];
printTest["unify[comp4, comp5]", unify[comp4, comp5], <|"X" -> "a", "Y" -> "b", "Z" -> "c"|>];

Print["\nPREDICATE TESTS:"];
printTest["unify[pred1, pred2]", unify[pred1, pred2], <|"X" -> "b", "Y" -> "a"|>];
printTest["unify[pred1, pred3]", unify[pred1, pred3], $Failed];

Print["\nLIST STRUCTURE TESTS:"];
printTest["unify[listStruct1, listStruct2]", 
  unify[listStruct1, listStruct2], 
  <|"Y" -> "a","X" -> "c"|>];
printTest["unify[listStruct1, listStruct1]", 
  unify[listStruct1, listStruct1], 
  <||>];

Print["\nMIXED TESTS:"];
mixed1 = <|"Compound" -> "pair", "Arguments" -> {varX, list1}|>;
mixed2 = <|"Compound" -> "pair", "Arguments" -> {constA, list3}|>;
printTest["unify[mixed1, mixed2]", 
  unify[mixed1, mixed2], 
  $Failed];

(* Test with nested compound terms *)
nested1 = <|"Compound" -> "f", "Arguments" -> {comp1, varY}|>;
nested2 = <|"Compound" -> "f", "Arguments" -> {comp2, constA}|>;
printTest["unify[nested1, nested2]", 
  unify[nested1, nested2], 
  <|"X" -> "b", "Y" -> "a"|>];
