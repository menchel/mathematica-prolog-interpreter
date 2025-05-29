(* unification *)
ClearAll[unify,preventInfinity,unifyHelper];
(* a helper function to make a subsitution *)
unifyHelper[term_, replacement_] := Which[
  StringQ[term] && StringMatchQ[term, RegularExpression["[A-Z_].*"]],
    (* it fits! search in dict *)
    Lookup[replacement, term, term],
  (* it might fit a compound variable, time to try inside! *)
  AssociationQ[term] && KeyExistsQ[term, "Compound"],
    <|"Compound" -> term["Compound"], 
      "Arguments" -> unifyHelper[#, replacement] & /@ term["Arguments"]|>,
  (* just normal atom, string or something. Maybe else? *)
  True, term
];

(* prevents infinity reduction when X->... X *)
preventInfinity[variable_, term_, replacement_] := Module[
  (* calculate changes if needed *)
  {t = unifyHelper[term, replacement]},
  Which[
    (* is OK *)
    variable === t, True,
    (* try to unify to prevent *)
    AssociationQ[t] && KeyExistsQ[t, "Compound"],
      MemberQ[unifyHelper[#, replacement] & /@ t["Arguments"], variable, \[Infinity]],
    True, False
  ]
];

(* main unify function *)
unify[term1_, term2_, replacement_] := Module[
  {t1 = unifyHelper[term1, replacement], t2 = unifyHelper[term2, replacement]},
  
  Which[
    (* if they are the same, we are good! *)
    t1 === t2,
      replacement,

    (* maybe term1 after help is a variable *)
    StringQ[t1] && StringMatchQ[t1, RegularExpression["[A-Z_].*"]],
      If[preventInfinity[t1, t2, replacement], Fail, Join[replacement, <|t1 -> t2|>]],

    (* maybe term2 after help is a variable *)
    StringQ[t2] && StringMatchQ[t2, RegularExpression["[A-Z_].*"]],
      If[preventInfinity[t2, t1, replacement], Fail, Join[replacement, <|t2 -> t1|>]],

    (* not the same or variables, then it must be compound *)
    AssociationQ[t1] && AssociationQ[t2] &&
    KeyExistsQ[t1, "Compound"] && KeyExistsQ[t2, "Compound"] &&
    t1["Compound"] === t2["Compound"] &&
    Length[t1["Arguments"]] === Length[t2["Arguments"]],
      (* big if, but basically we check that both are indeed compund *)
      (* if so, we need to do unify on every single element inside *)
      (* so, use unify again! *)
      Fold[
        Function[{currSubst, pair},
          If[currSubst === Fail, Fail, unify[pair[[1]], pair[[2]], currSubst]]
        ],
        replacement,
        Transpose[{t1["Arguments"], t2["Arguments"]}]
      ],

    (* if we have gotten this far, then something is wrong *)
    True, Fail
  ]
];
