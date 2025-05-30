(* unify *)
ClearAll[unify, occursCheck, applySubstitution];

(* Apply substitution to a term *)
applySubstitution[term_, substitution_] := 
  Which[
    (* Variable with substitution *)
    StringQ[term] && term =!= "_" && StringMatchQ[term, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]] && KeyExistsQ[substitution, term],
      applySubstitution[substitution[term], substitution],
    (* Placeholder - don't substitute *)
    term === "_", "_",
    (* Compound term *)
    AssociationQ[term] && KeyExistsQ[term, "Compound"],
      <|"Compound" -> term["Compound"], 
        "Arguments" -> (applySubstitution[#, substitution] & /@ term["Arguments"])|>,
    (* Predicate *)
    AssociationQ[term] && KeyExistsQ[term, "arguments"],
      <|"head" -> term["head"], 
        "arguments" -> (applySubstitution[#, substitution] & /@ term["arguments"])|>,
    (* List structure *)
    AssociationQ[term] && KeyExistsQ[term, "ListHead"],
      <|"ListHead" -> applySubstitution[term["ListHead"], substitution], 
        "Tail" -> applySubstitution[term["Tail"], substitution]|>,
    (* Regular list *)
    ListQ[term],
      applySubstitution[#, substitution] & /@ term,
    (* Default case *)
    True, term
  ];

(* Check if variable occurs in term *)
occursCheck[var_, term_, substitution_] := Module[
  {t = applySubstitution[term, substitution]},
  Which[
    var === t, True,
    t === "_", False,
    AssociationQ[t] && KeyExistsQ[t, "Compound"],
      AnyTrue[t["Arguments"], occursCheck[var, #, substitution] &],
    AssociationQ[t] && KeyExistsQ[t, "arguments"],
      AnyTrue[t["arguments"], occursCheck[var, #, substitution] &],
    AssociationQ[t] && KeyExistsQ[t, "ListHead"],
      occursCheck[var, t["ListHead"], substitution] || occursCheck[var, t["Tail"], substitution],
    ListQ[t],
      AnyTrue[t, occursCheck[var, #, substitution] &],
    True, False
  ]
];

(* Main unification function *)
unify[term1_, term2_, substitution_:<||>] := Module[
  {t1 = applySubstitution[term1, substitution], 
   t2 = applySubstitution[term2, substitution]},
  
  Which[
    (* Terms are identical *)
    t1 === t2, substitution,
    
    (* Either term is a placeholder *)
    t1 === "_", substitution,
    t2 === "_", substitution,
    
    (* t1 is a variable *)
    StringQ[t1] && t1 =!= "_" && StringMatchQ[t1, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]],
      If[occursCheck[t1, t2, substitution], 
        $Failed,
        Join[substitution, <|t1 -> t2|>]
      ],
    
    (* t2 is a variable *)
    StringQ[t2] && t2 =!= "_" && StringMatchQ[t2, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]],
      If[occursCheck[t2, t1, substitution], 
        $Failed,
        Join[substitution, <|t2 -> t1|>]
      ],
    
    (* Both are lists *)
    ListQ[t1] && ListQ[t2] && Length[t1] === Length[t2],
      Fold[
        Function[{currSubst, pair}, 
          If[currSubst === $Failed, $Failed, unify[pair[[1]], pair[[2]], currSubst]]
        ],
        substitution,
        Transpose[{t1, t2}]
      ],
    
    (* Both are list structures *)
    AssociationQ[t1] && AssociationQ[t2] && 
    KeyExistsQ[t1, "ListHead"] && KeyExistsQ[t2, "ListHead"],
      Module[{headSubst = unify[t1["ListHead"], t2["ListHead"], substitution]},
        If[headSubst === $Failed, $Failed, unify[t1["Tail"], t2["Tail"], headSubst]]
      ],
    
    (* Both are compound terms *)
    AssociationQ[t1] && AssociationQ[t2] &&
    KeyExistsQ[t1, "Compound"] && KeyExistsQ[t2, "Compound"] &&
    t1["Compound"] === t2["Compound"] &&
    Length[t1["Arguments"]] === Length[t2["Arguments"]],
      Fold[
        Function[{currSubst, pair}, 
          If[currSubst === $Failed, $Failed, unify[pair[[1]], pair[[2]], currSubst]]
        ],
        substitution,
        Transpose[{t1["Arguments"], t2["Arguments"]}]
      ],
    
    (* Both are predicates *)
    AssociationQ[t1] && AssociationQ[t2] &&
    KeyExistsQ[t1, "head"] && KeyExistsQ[t2, "head"] &&
    t1["head"] === t2["head"] &&
    Length[t1["arguments"]] === Length[t2["arguments"]],
      Fold[
        Function[{currSubst, pair}, 
          If[currSubst === $Failed, $Failed, unify[pair[[1]], pair[[2]], currSubst]]
        ],
        substitution,
        Transpose[{t1["arguments"], t2["arguments"]}]
      ],
    
    (* Default: unification failed *)
    True, $Failed
  ]
];
