(* ============ unify ============= *)
(*
	The unify gets a query and a term
	, and tries to compare them, infering connections of variables on the way
*)
(* ==================================== *)

(* unify *)
ClearAll[unify, checkInfinity, makeSubstitution];

(*makes a subsitution for a term *)
makeSubstitution[term_, substitution_] := 
  Which[
    StringQ[term] && term =!= "_" && StringMatchQ[term, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]] && KeyExistsQ[substitution, term],
      makeSubstitution[substitution[term], substitution], (* Variable with substitution, replace! *)
    term === "_", "_",     (* Placeholder *)
    AssociationQ[term] && KeyExistsQ[term, "Compound"], (* Compound *)
      <|"Compound" -> term["Compound"], 
        "Arguments" -> (makeSubstitution[#, substitution] & /@ term["Arguments"])|>,
    AssociationQ[term] && KeyExistsQ[term, "arguments"], (* predicate *)
      <|"head" -> term["head"], 
        "arguments" -> (makeSubstitution[#, substitution] & /@ term["arguments"])|>,
    AssociationQ[term] && KeyExistsQ[term, "ListHead"], (* a wild list has appeared *)
      <|"ListHead" -> makeSubstitution[term["ListHead"], substitution], 
        "Tail" -> makeSubstitution[term["Tail"], substitution]|>,
    ListQ[term], (* not so wild list? *)
      makeSubstitution[#, substitution] & /@ term,
    True, term (* others-> just return as normal *)
  ];

(* Check if variable occurs in term *)
checkInfinity[var_, term_, substitution_] := Module[
  {t = makeSubstitution[term, substitution]},
  Which[ (* go over all of the cases *)
    var === t, True, (* if atom, then isn't *)
    t === "_", False, (* place holder-> problem *)
    AssociationQ[t] && KeyExistsQ[t, "Compound"], (* compound term, we need to dive deeper *)
      AnyTrue[t["Arguments"], checkInfinity[var, #, substitution] &],
    AssociationQ[t] && KeyExistsQ[t, "arguments"], (* a list of arguments, we need to dive deeper *)
      AnyTrue[t["arguments"], checkInfinity[var, #, substitution] &],
    AssociationQ[t] && KeyExistsQ[t, "ListHead"], (* a list, we need to dive deeper *)
      checkInfinity[var, t["ListHead"], substitution] || checkInfinity[var, t["Tail"], substitution],
    ListQ[t], (* just list of stuff, check that all are finit *)
      AnyTrue[t, checkInfinity[var, #, substitution] &],
    True, False (* nothing happaned, good! *)
  ]
];

(* The main event-> The unify function! *)
unify[term1_, term2_, substitution_:<||>] := Module[
  {t1 = makeSubstitution[term1, substitution], 
   t2 = makeSubstitution[term2, substitution]},
  Which[
    t1 === t2, substitution, (* terms are the same *)
    
    t1 === "_", substitution, (* placeholder-> special case *)
    t2 === "_", substitution,
    
    StringQ[t1] && t1 =!= "_" && StringMatchQ[t1, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]], (* t1 is a variable *)
      If[checkInfinity[t1, t2, substitution], 
        $Failed,
        Join[substitution, <|t1 -> t2|>]
      ],
    
    StringQ[t2] && t2 =!= "_" && StringMatchQ[t2, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]], (* t2 is a variable *)
      If[checkInfinity[t2, t1, substitution], 
        $Failed,
        Join[substitution, <|t2 -> t1|>]
      ],
    ListQ[t1] && ListQ[t2] && Length[t1] === Length[t2], (* maybe the 2 are just lists *)
      Fold[ (* fold, since we accumulate what we know so far. SML vibes *)
        Function[{currSubst, pair}, 
          If[currSubst === $Failed, $Failed, unify[pair[[1]], pair[[2]], currSubst]]
        ],
        substitution,
        Transpose[{t1, t2}] (* saw a post that said that transpose here could help, it did *)
      ],
    
    AssociationQ[t1] && AssociationQ[t2] &&  (* maybe they are prolog lists? *)
    KeyExistsQ[t1, "ListHead"] && KeyExistsQ[t2, "ListHead"],
      Module[{headSubst = unify[t1["ListHead"], t2["ListHead"], substitution]},
        If[headSubst === $Failed, $Failed, unify[t1["Tail"], t2["Tail"], headSubst]]
      ],
      
    AssociationQ[t1] && KeyExistsQ[t1, "ListHead"] && ListQ[t2] && Length[t2] > 0, (* maybe t1 is [X|Z] and t2 is {...} *)
    unify[t1, <|"ListHead" -> First[t2], "Tail" -> Rest[t2]|>, substitution],
    
    ListQ[t1] && Length[t1] > 0 && AssociationQ[t2] && KeyExistsQ[t2, "ListHead"], (* maybe t2 is [X|Z] and t1 is {...} *)
    unify[<|"ListHead" -> First[t1], "Tail" -> Rest[t1]|>, t2, substitution],
    
    AssociationQ[t1] && AssociationQ[t2] && (* maybe they are both compund? *)
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
    
    AssociationQ[t1] && AssociationQ[t2] && (* maybe they are both regular predicates *)
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
    
    True, $Failed (* maybe nothing, so that is a problem *)
  ]
];
