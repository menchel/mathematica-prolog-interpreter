(* ============ renamer ============= *)
(*
	The renamer gets a parsed fact or rule from the dictionary
	, and creates a new version with fresh variables
*)
(* ==================================== *)

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
    StringQ[term] && StringMatchQ[term, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]], (* case 1: variable *)
      {term},
    AssociationQ[term] && KeyExistsQ[term, "Compound"], (* case 2: compound term *)
      Flatten[variableCollector /@ term["Arguments"]],
    AssociationQ[term] && KeyExistsQ[term, "arguments"], (* case 3: predicate arguments *)
      Flatten[variableCollector /@ term["arguments"]],
    AssociationQ[term] && KeyExistsQ[term, "Negation"], (* case 4: negation *)
      variableCollector[term["Negation"]],
    AssociationQ[term] && KeyExistsQ[term, "ListHead"], (* case 5: list with head/tail *)
      Join[variableCollector[term["ListHead"]], variableCollector[term["Tail"]]],
    Head[term] === Rule, (* case 6: rule (->) *)
      Join[
        variableCollector[term[[1]]],  (* left side *)
        Flatten[variableCollector /@ Flatten[term[[2]], 1]]  (* right side: flatten one level and collect *)
      ],
    ListQ[term], (* case 7: regular list *)
      Flatten[variableCollector /@ term],
    True, {} (* default *)
  ];

variableReplacer[term_, variablesMapping_] := 
  Which[
    StringQ[term] && KeyExistsQ[variablesMapping, term], (* variable *)
      variablesMapping[term],
    AssociationQ[term] && KeyExistsQ[term, "Compound"], (* compound term *)
      <|"Compound" -> term["Compound"], "Arguments" -> (variableReplacer[#, variablesMapping] & /@ term["Arguments"])|>,
    AssociationQ[term] && KeyExistsQ[term, "arguments"], (* predicate *)
      <|"head" -> term["head"], "arguments" -> (variableReplacer[#, variablesMapping] & /@ term["arguments"])|>,
    AssociationQ[term] && KeyExistsQ[term, "Negation"], (* negation *)
      <|"Negation" -> variableReplacer[term["Negation"], variablesMapping]|>,
    AssociationQ[term] && KeyExistsQ[term, "ListHead"], (* list with head/tail *)
      <|"ListHead" -> variableReplacer[term["ListHead"], variablesMapping], "Tail" -> variableReplacer[term["Tail"], variablesMapping]|>,
    Head[term] === Rule, (* rule (->) *)
      Rule[
        variableReplacer[term[[1]], variablesMapping], 
        (variableReplacer[#, variablesMapping] & /@ #) & /@ term[[2]]
      ],
    ListQ[term], (* regular list *)
      variableReplacer[#, variablesMapping] & /@ term,
    True, term (* default *)
  ];

variableRenamer[clause_] := Module[
  {variables, variableMapping, newClause},
  variables = DeleteDuplicates[variableCollector[clause]]; (* collect all variables *)
  variableMapping = AssociationThread[variables -> (uniqueVarGenerator[] & /@ variables)];   (* create mapping to fresh variables *)
  newClause = variableReplacer[clause, variableMapping];   (* replace variables in clause *)
  newClause
];
