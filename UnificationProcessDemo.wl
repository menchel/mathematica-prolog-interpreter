(* ::Package:: *)

(* my unify code *)
makeSubstitution[term_, substitution_] := 
  Which[
    StringQ[term] && term =!= "_" && StringMatchQ[term, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]] && KeyExistsQ[substitution, term],
      makeSubstitution[substitution[term], substitution],
    term === "_", "_",
    AssociationQ[term] && KeyExistsQ[term, "Compound"],
      <|"Compound" -> term["Compound"], 
        "Arguments" -> (makeSubstitution[#, substitution] & /@ term["Arguments"])|>,
    AssociationQ[term] && KeyExistsQ[term, "arguments"],
      <|"head" -> term["head"], 
        "arguments" -> (makeSubstitution[#, substitution] & /@ term["arguments"])|>,
    AssociationQ[term] && KeyExistsQ[term, "ListHead"],
      <|"ListHead" -> makeSubstitution[term["ListHead"], substitution], 
        "Tail" -> makeSubstitution[term["Tail"], substitution]|>,
    ListQ[term],
      makeSubstitution[#, substitution] & /@ term,
    True, term
  ];

checkInfinity[var_, term_, substitution_] := Module[
  {t = makeSubstitution[term, substitution]},
  Which[
    var === t, True,
    t === "_", False,
    AssociationQ[t] && KeyExistsQ[t, "Compound"],
      AnyTrue[t["Arguments"], checkInfinity[var, #, substitution] &],
    AssociationQ[t] && KeyExistsQ[t, "arguments"],
      AnyTrue[t["arguments"], checkInfinity[var, #, substitution] &],
    AssociationQ[t] && KeyExistsQ[t, "ListHead"],
      checkInfinity[var, t["ListHead"], substitution] || checkInfinity[var, t["Tail"], substitution],
    ListQ[t],
      AnyTrue[t, checkInfinity[var, #, substitution] &],
    True, False
  ]
];

(* some changes to track the progress! *)
ClearAll[unifyLogged]
SetAttributes[unifyLogged, HoldAll];

unifyLogged[t1_, t2_] := Module[
  {log = {}, result},

  logStep[term1_, term2_, subst_, status_] := 
    AppendTo[log, <|
      "Term1" -> term1,
      "Term2" -> term2,
      "Substitution" -> subst,
      "Status" -> status
    |>];

  ClearAll[loggedUnify];
  loggedUnify[term1_, term2_, substitution_:<||>] := Module[
    {lhs = makeSubstitution[term1, substitution], 
     rhs = makeSubstitution[term2, substitution], subst = substitution, res},

    logStep[lhs, rhs, subst, "Comparing"];

    res = Which[
      lhs === rhs, subst,
      lhs === "_" || rhs === "_", subst,

      StringQ[lhs] && lhs =!= "_" && StringMatchQ[lhs, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]],
        If[checkInfinity[lhs, rhs, subst],
          logStep[lhs, rhs, subst, "Occurs check failed"]; $Failed,
          logStep[lhs, rhs, Join[subst, <|lhs -> rhs|>], "Substituted"];
          Join[subst, <|lhs -> rhs|>]
        ],

      StringQ[rhs] && rhs =!= "_" && StringMatchQ[rhs, RegularExpression["[A-Z_][a-zA-Z0-9_]*"]],
        If[checkInfinity[rhs, lhs, subst],
          logStep[rhs, lhs, subst, "Occurs check failed"]; $Failed,
          logStep[rhs, lhs, Join[subst, <|rhs -> lhs|>], "Substituted"];
          Join[subst, <|rhs -> lhs|>]
        ],

      ListQ[lhs] && ListQ[rhs] && Length[lhs] === Length[rhs],
        Fold[
          Function[{currSubst, pair},
            If[currSubst === $Failed, $Failed, loggedUnify[pair[[1]], pair[[2]], currSubst]]
          ],
          subst,
          Transpose[{lhs, rhs}]
        ],

      AssociationQ[lhs] && KeyExistsQ[lhs, "ListHead"] && ListQ[rhs],
        loggedUnify[lhs, <|"ListHead" -> First[rhs], "Tail" -> Rest[rhs]|>, subst],

      ListQ[lhs] && AssociationQ[rhs] && KeyExistsQ[rhs, "ListHead"],
        loggedUnify[<|"ListHead" -> First[lhs], "Tail" -> Rest[lhs]|>, rhs, subst],

      AssociationQ[lhs] && AssociationQ[rhs] && KeyExistsQ[lhs, "ListHead"] && KeyExistsQ[rhs, "ListHead"],
        Module[{h = loggedUnify[lhs["ListHead"], rhs["ListHead"], subst]},
          If[h === $Failed, $Failed, loggedUnify[lhs["Tail"], rhs["Tail"], h]]
        ],

      AssociationQ[lhs] && AssociationQ[rhs] && KeyExistsQ[lhs, "Compound"] &&
      KeyExistsQ[rhs, "Compound"] && lhs["Compound"] === rhs["Compound"] &&
      Length[lhs["Arguments"]] === Length[rhs["Arguments"]],
        Fold[
          Function[{currSubst, pair},
            If[currSubst === $Failed, $Failed, loggedUnify[pair[[1]], pair[[2]], currSubst]]
          ],
          subst,
          Transpose[{lhs["Arguments"], rhs["Arguments"]}]
        ],

      AssociationQ[lhs] && AssociationQ[rhs] && KeyExistsQ[lhs, "head"] && KeyExistsQ[rhs, "head"] &&
      lhs["head"] === rhs["head"] && Length[lhs["arguments"]] === Length[rhs["arguments"]],
        Fold[
          Function[{currSubst, pair},
            If[currSubst === $Failed, $Failed, loggedUnify[pair[[1]], pair[[2]], currSubst]]
          ],
          subst,
          Transpose[{lhs["arguments"], rhs["arguments"]}]
        ],

      True, logStep[lhs, rhs, subst, "Failed"]; $Failed
    ];

    res
  ];

  result = loggedUnify[t1, t2, <||>];
  <|"Result" -> result, "Log" -> log|>
];

(* Some nice tools to present it nicely! *)
ClearAll[toAST]
toAST[term_] := Which[
  StringQ[term], term,
  AssociationQ[term] && KeyExistsQ[term, "Compound"],
    TreeForm[TreeNode[term["Compound"], toAST /@ term["Arguments"]]],
  AssociationQ[term] && KeyExistsQ[term, "head"],
    TreeForm[TreeNode[term["head"], toAST /@ term["arguments"]]],
  AssociationQ[term] && KeyExistsQ[term, "ListHead"],
    TreeForm[TreeNode["[|]", {toAST[term["ListHead"]], toAST[term["Tail"]]}]],
  ListQ[term],
    TreeForm[TreeNode["List", toAST /@ term]],
  True, term
];

(* a bit of visual tools! *)
visualizeUnification[logData_] := Manipulate[
  Module[{step = logData[[i]]},
    Grid[{
      {Style["Term 1", Bold], toAST[step["Term1"]]},
      {Style["Term 2", Bold], toAST[step["Term2"]]},
      {Style["Current attempt", Bold], step["Substitution"]},
      {Style["Result", Bold], step["Status"]}
    }, Alignment -> Left, Spacings -> {2, 2}]
  ],
  {{i, 1, "Step"}, 1, Length[logData], 1}
];

(* some examples *)
example1= unifyLogged[
  <|"Compound" -> "f", "Arguments" -> {"X", <|"Compound" -> "g", "Arguments" -> {"Y"}|> }|>,
  <|"Compound" -> "f", "Arguments" -> {"a", <|"Compound" -> "g", "Arguments" -> {"b"}|> }|>
];

example2 = unifyLogged[
  <|"Compound" -> "f", "Arguments" -> {
    <|"Compound" -> "g", "Arguments" -> {"X", "Y"}|>,
    <|"ListHead" -> "Z", "Tail" -> {"W"}|>
  }|>,
  
  <|"Compound" -> "f", "Arguments" -> {
    <|"Compound" -> "g", "Arguments" -> {"a", "b"}|>,
    {"c", "d"}
  }|>
];

(* example of a failed unification *)
example3 = unifyLogged[
  <|"Compound" -> "f", "Arguments" -> {
    <|"Compound" -> "g", "Arguments" -> {"X", "Y"}|>,
    <|"ListHead" -> "Z", "Tail" -> {"e"}|>
  }|>,
  
  <|"Compound" -> "f", "Arguments" -> {
    <|"Compound" -> "g", "Arguments" -> {"a", "b"}|>,
    {"c", "d"}
  }|>
];


visualizeUnification[example2["Log"]]
