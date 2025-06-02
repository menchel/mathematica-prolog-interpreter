(* unify *)
ClearAll[unify, checkInfinity, makeSubstitution];

(*makes a subsitution for a term *)
makeSubstitution[term_, substitution_] := 
  Which[
    (* Variable with substitution, replace! *)
    StringQ[term] && term =!= "_" && StringMatchQ[term, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]] && KeyExistsQ[substitution, term],
      makeSubstitution[substitution[term], substitution],
    (* Placeholder *)
    term === "_", "_",
    (* Compound *)
    AssociationQ[term] && KeyExistsQ[term, "Compound"],
      <|"Compound" -> term["Compound"], 
        "Arguments" -> (makeSubstitution[#, substitution] & /@ term["Arguments"])|>,
    (* predicate *)
    AssociationQ[term] && KeyExistsQ[term, "arguments"],
      <|"head" -> term["head"], 
        "arguments" -> (makeSubstitution[#, substitution] & /@ term["arguments"])|>,
    (* a wild list has appeared *)
    AssociationQ[term] && KeyExistsQ[term, "ListHead"],
      <|"ListHead" -> makeSubstitution[term["ListHead"], substitution], 
        "Tail" -> makeSubstitution[term["Tail"], substitution]|>,
    (* not so wild list? *)
    ListQ[term],
      makeSubstitution[#, substitution] & /@ term,
    (* others-> just return as normal *)
    True, term
  ];

(* Check if variable occurs in term *)
checkInfinity[var_, term_, substitution_] := Module[
  {t = makeSubstitution[term, substitution]},
  (* go over all of the cases *)
  Which[
    (* if atom, then isn't *)
    var === t, True,
    (* place holder-> problem *)
    t === "_", False,
    (* compound term, we need to dive deeper *)
    AssociationQ[t] && KeyExistsQ[t, "Compound"],
      AnyTrue[t["Arguments"], checkInfinity[var, #, substitution] &],
    (* a list of arguments, we need to dive deeper *)
    AssociationQ[t] && KeyExistsQ[t, "arguments"],
      AnyTrue[t["arguments"], checkInfinity[var, #, substitution] &],
    (* a list, we need to dive deeper *)
    AssociationQ[t] && KeyExistsQ[t, "ListHead"],
      checkInfinity[var, t["ListHead"], substitution] || checkInfinity[var, t["Tail"], substitution],
    (* just list of stuff, check that all are finit *)
    ListQ[t],
      AnyTrue[t, checkInfinity[var, #, substitution] &],
    (* nothing happaned, good! *)
    True, False
  ]
];

(* The main event-> The unify function! *)
unify[term1_, term2_, substitution_:<||>] := Module[
  {t1 = makeSubstitution[term1, substitution], 
   t2 = makeSubstitution[term2, substitution]},
  Which[
    (* terms are the same *)
    t1 === t2, substitution,
    
    (* placeholder-> special case *)
    t1 === "_", substitution,
    t2 === "_", substitution,
    
    (* t1 is a variable *)
    StringQ[t1] && t1 =!= "_" && StringMatchQ[t1, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]],
      If[checkInfinity[t1, t2, substitution], 
        $Failed,
        Join[substitution, <|t1 -> t2|>]
      ],
    
    (* t2 is a variable *)
    StringQ[t2] && t2 =!= "_" && StringMatchQ[t2, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]],
      If[checkInfinity[t2, t1, substitution], 
        $Failed,
        Join[substitution, <|t2 -> t1|>]
      ],
    (* maybe the 2 are just lists *)
    ListQ[t1] && ListQ[t2] && Length[t1] === Length[t2],
      (* fold, since we accumulate what we know so far. SML vibes *)
      Fold[
        Function[{currSubst, pair}, 
          If[currSubst === $Failed, $Failed, unify[pair[[1]], pair[[2]], currSubst]]
        ],
        substitution,
        (* saw a post that said that transpose here could help, it did *)
        Transpose[{t1, t2}]
      ],
    
    (* maybe they are prolog lists? *)
    AssociationQ[t1] && AssociationQ[t2] && 
    KeyExistsQ[t1, "ListHead"] && KeyExistsQ[t2, "ListHead"],
      Module[{headSubst = unify[t1["ListHead"], t2["ListHead"], substitution]},
        If[headSubst === $Failed, $Failed, unify[t1["Tail"], t2["Tail"], headSubst]]
      ],
      
    (* maybe t1 is [X|Z] and t2 is {...} *)
    AssociationQ[t1] && KeyExistsQ[t1, "ListHead"] && ListQ[t2] && Length[t2] > 0,
    unify[t1, <|"ListHead" -> First[t2], "Tail" -> Rest[t2]|>, substitution],
    
    (* maybe t2 is [X|Z] and t1 is {...} *)
    ListQ[t1] && Length[t1] > 0 && AssociationQ[t2] && KeyExistsQ[t2, "ListHead"],
    unify[<|"ListHead" -> First[t1], "Tail" -> Rest[t1]|>, t2, substitution],
    
    (* maybe they are both compund? *)
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
    
    (* maybe they are both regular predicates *)
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
    
    (* maybe nothing, so that is a problem *)
    True, $Failed
  ]
];
