(* renamer *)
(* first, we need to create a variable name change, to prevent collition *)
ClearAll[variableRenamer,variableReplacer,variableCollector]
(* we need: 
1) collect all of the variables
2) replace all of the variables
3) using that, rename them all *)

(* 1) collect all of the variables *)
variableCollector[term_] := 
  Which[
    (* case 1: just a regular term *)
    StringQ[term] && StringMatchQ[term, RegularExpression["[A-Z_].*"]],
      {term},
    (* case 2: compund arguments-> so it is a list of terms *)
    AssociationQ[term] && KeyExistsQ[term, "Compound"],
      Flatten[variableCollector /@ term["Arguments"]],
    (* case 2: arguments-> so it is a list of terms *)
    AssociationQ[term] && KeyExistsQ[term, "arguments"],
      Flatten[variableCollector /@ term["arguments"]],
    
    (* case 3: negation, means that a term is coming *)
    AssociationQ[term] && KeyExistsQ[term, "Negation"],
      variableCollector[term["Negation"]],
    (* case 4: we have a list :) *)
    AssociationQ[term] && KeyExistsQ[term, "ListHead"],
      Join[variableCollector[term["ListHead"]], variableCollector[term["Tail"]]],
    
    (* another list *)
    ListQ[term],
      Flatten[variableCollector /@ term],
    
    (* maybe no variable *)
    True, {}
  ];


(* replacing the variables *)
variableReplacer[term_, variablesMapping_] := 
  Which[
    (* just a term *)
    StringQ[term] && KeyExistsQ[variablesMapping, term],
      variablesMapping[term],
    (* compound term *)
    AssociationQ[term] && KeyExistsQ[term, "Compound"],
      <|"Compound" -> term["Compound"], 
        "Arguments" -> variableReplacer[#, variablesMapping] & /@ term["Arguments"]|>,
    (* arguments, so kind of list *)
    AssociationQ[term] && KeyExistsQ[term, "arguments"],
      AssociationMap[variableReplacer[#, variablesMapping] &, term],
    (* negation-> maybe a variable soon? *)
    AssociationQ[term] && KeyExistsQ[term, "Negation"],
      <|"Negation" -> variableReplacer[term["Negation"], variablesMapping]|>,
    (* a list, hopefully variables are coming *)
    AssociationQ[term] && KeyExistsQ[term, "ListHead"],
      <|"ListHead" -> variableReplacer[term["ListHead"], variablesMapping], 
        "Tail" -> variableReplacer[term["Tail"], variablesMapping]|>,
    (* another list *)
    ListQ[term],
      variableReplacer[#, variablesMapping] & /@ term,
    (* default *)
    True, term
  ];

variableRenamer[clause_] := Module[
  {newVars, variableMapping, rename, newClause},
  
  (* collect all of the variables *)
  newVars = DeleteDuplicates[variableCollector[clause]];
  (* take each variable to a unique name *)
  Print[newVars]
  variableMapping = {};
  
  (* replace all of the variables everywhere *)
  newClause = variableReplacer[clause, variableMapping];
  newClause
];
