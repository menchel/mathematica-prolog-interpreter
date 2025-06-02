Print["Test 1: Simple predicate with two distinct variables"];
input1 = <|"head" -> "parent", "arguments" -> {"X", "Y"}|>;
Print[input1];
Print[variableRenamer[input1]];

Print["\nTest 2: Predicate with repeated variables"];
input2 = <|"head" -> "sibling", "arguments" -> {"X", "X"}|>;
Print[input2];
Print[variableRenamer[input2]];

Print["\nTest 3: Nested compound term with variables"];
input3 = <|"Compound" -> "f", "Arguments" -> {"X", <|"Compound" -> "g", "Arguments" -> {"Y"}|>}|>;
Print[input3];
Print[variableRenamer[input3]];

Print["\nTest 4: List with head and tail variables"];
Print[input4];
input4 = <|"ListHead" -> "X", "Tail" -> "Y"|>;
Print[variableRenamer[input4]];

Print["\nTest 5: Negation with variable inside"];
input5 = <|"Negation" -> <|"head" -> "human", "arguments" -> {"X"}|>|>;
Print[input5];
Print[variableRenamer[input5]];

Print["\nTest 6: Multiple predicates with nested terms"];
input6 = {
  <|"head" -> "likes", "arguments" -> {"X", <|"Compound" -> "icecream", "Arguments" -> {"Y"}|>}|>,
  <|"head" -> "dislikes", "arguments" -> {"Z"}|>
};
Print[input6];
Print[variableRenamer[input6]];

Print["\nTest 7: Multiple predicates with nested terms but same val"];
input7 = {
  <|"head" -> "likes", "arguments" -> {"X", <|"Compound" -> "icecream", "Arguments" -> {"X"}|>}|>,
  <|"head" -> "dislikes", "arguments" -> {"X"}|>
};
Print[input7];
Print[variableRenamer[input7]];
Print["\nTest 8: Deeply nested compound and predicate mix"];
input8 = <|"head" -> "knows", 
  "arguments" -> {
    <|"Compound" -> "f", "Arguments" -> {
      <|"Compound" -> "g", "Arguments" -> {"X"}|>,
      "Y"
    }|>,
    <|"Compound" -> "h", "Arguments" -> {
      <|"head" -> "likes", "arguments" -> {"Z", "X"}|>
    }|>
  }
|>;
Print[input8];
Print[variableRenamer[input8]];

Print["\nTest 9: List structure nested in compound"];
input9 = <|"Compound" -> "wrap", "Arguments" -> {
  <|"ListHead" -> "X", "Tail" -> <|"ListHead" -> "Y", "Tail" -> "Z"|>|>
}|>;
Print[input9];
Print[variableRenamer[input9]];

Print["\nTest 10: Multiple clauses with overlapping variables"];
input10 = {
  <|"head" -> "p", "arguments" -> {"X", "Y"}|>,
  <|"head" -> "q", "arguments" -> {"Y", "Z"}|>,
  <|"head" -> "r", "arguments" -> {"X", "Z"}|>
};
Print[input10];
Print[variableRenamer[input10]];

Print["\nTest 11: Compound inside negation and predicate"];
input11 = <|"Negation" -> <|
  "head" -> "owns",
  "arguments" -> {
    <|"Compound" -> "car", "Arguments" -> {"X"}|>,
    "Y"
  }
|>|>;
Print[input11];
Print[variableRenamer[input11]];

Print["\nTest 12: Lists with variables, lists as arguments"];
input12 = <|"head" -> "combine", "arguments" -> {
  {"X", "Y", "Z"},
  <|"ListHead" -> "A", "Tail" -> {"B", "C"}|>
}|>;
Print[input12];
Print[variableRenamer[input12]];

Print["\nTest 13: Repeated variable in deep branches"];
input13 = <|"Compound" -> "tree", "Arguments" -> {
  <|"Compound" -> "node", "Arguments" -> {"X", <|"Compound" -> "leaf", "Arguments" -> {"X"}|>}|>,
  <|"Compound" -> "node", "Arguments" -> {"Y", "Z"}|>
}|>;
Print[input13];
Print[variableRenamer[input13]];
