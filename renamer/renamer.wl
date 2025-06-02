(* renamer *)
ClearAll[variableRenamer, variableReplacer, variableCollector, uniqueVar];

(* create unique names each time *)
uniqueVar[] := Module[{count = 0}, 
  Function[Null, SymbolName[Symbol["TT" <> ToString[count++]]]]
];
uniqueVarGenerator = uniqueVar[];

(* 1) collect all of the variables *)
variableCollector[term_] := 
  Which[
    (* case 1: just a regular variable *)
    StringQ[term] && StringMatchQ[term, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]],
      {term},
    (* case 2: compound term *)
    AssociationQ[term] && KeyExistsQ[term, "Compound"],
      Flatten[variableCollector /@ term["Arguments"]],
    (* case 3: predicate arguments *)
    AssociationQ[term] && KeyExistsQ[term, "arguments"],
      Flatten[variableCollector /@ term["arguments"]],
    (* case 4: negation *)
    AssociationQ[term] && KeyExistsQ[term, "Negation"],
      variableCollector[term["Negation"]],
    (* case 5: list with head/tail *)
    AssociationQ[term] && KeyExistsQ[term, "ListHead"],
      Join[variableCollector[term["ListHead"]], variableCollector[term["Tail"]]],
    (* case 6: regular list *)
    ListQ[term],
      Flatten[variableCollector /@ term],
    (* default: no variables *)
    True, {}
  ];

(* replacing the variables *)
variableReplacer[term_, variablesMapping_] := 
  Which[
    (* variable *)
    StringQ[term] && KeyExistsQ[variablesMapping, term],
      variablesMapping[term],
    (* compound term *)
    AssociationQ[term] && KeyExistsQ[term, "Compound"],
       temp = variableReplacer[#, variablesMapping] & /@ term["Arguments"];
      <|"Compound" -> term["Compound"], 
        "Arguments" -> temp|>,
    (* predicate *)
    AssociationQ[term] && KeyExistsQ[term, "arguments"],
       temp = variableReplacer[#, variablesMapping] & /@ term["arguments"];
      <|"head" -> term["head"], 
        "arguments" -> temp|>,
    (* negation *)
    AssociationQ[term] && KeyExistsQ[term, "Negation"],
      <|"Negation" -> variableReplacer[term["Negation"], variablesMapping]|>,
    (* list with head/tail *)
    AssociationQ[term] && KeyExistsQ[term, "ListHead"],
      <|"ListHead" -> variableReplacer[term["ListHead"], variablesMapping], 
        "Tail" -> variableReplacer[term["Tail"], variablesMapping]|>,
    (* regular list *)
    ListQ[term],
      variableReplacer[#, variablesMapping] & /@ term,
    (* default *)
    True, term
  ];

variableRenamer[clause_] := Module[
  {variables, variableMapping, newClause},
  
  (* collect all variables *)
  variables = DeleteDuplicates[variableCollector[clause]];
  (* create mapping to fresh variables *)
  variableMapping = AssociationThread[variables -> (uniqueVarGenerator[] & /@ variables)];
  (* replace variables in clause *)
  newClause = variableReplacer[clause, variableMapping];
  newClause
];
