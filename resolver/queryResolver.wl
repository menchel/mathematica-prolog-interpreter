(* query resolver *)
ClearAll[resolveQuery];

(* main resolve *)
resolveQuery[queries_, db_] := Module[{results = {}},
  (* unique number for each *)
  uniqueVarGenerator = uniqueVar[];
  (* take care of all query *)
  Do[
    (* a single query *)
    Module[{solutions = resolveSingleQuery[q[[1]], db]},
      If[solutions === {} || solutions === $Failed,
        Print["  No solutions found"],
        (* print all of the solutions *)
        Do[
          Print["  Solution " <> ToString[solNum] <> ": " <> formatSolution[sol]],
          {sol, solutions}, {solNum, Length[solutions]}
        ]
      ];
      AppendTo[results, solutions]
    ];
    Print[""],
    {q, queries}
  ];
  results
];

(* a helper for resolving a single query *)
resolveSingleQuery[query_, db_] := resolvePredicateList[query, db, <||>];

(* resolves a list of predicates *)
resolvePredicateList[predicateList_, db_, substitution_] := Module[{
    firstPredicate = First[predicateList], 
    restPredicates = Rest[predicateList]
  },
  (* if no predicate to solve *)
  If[predicateList === {}, Return[{substitution}]];
  (* we have predicates *)
  Module[{firstSolutions = resolveSinglePredicate[firstPredicate, db, substitution]},
    If[firstSolutions === $Failed || firstSolutions === {}, Return[{}]];
    Join @@ Map[
      Function[solution,
        If[restPredicates === {},
          {solution},
          resolvePredicateList[restPredicates, db, solution]
        ]
      ],
      firstSolutions
    ]
  ]
];

(* resolve a single predicates *)
resolveSinglePredicate[predicates_, db_, substitution_] := Module[
  {solutions = {}, originalVariables, headUnified, bodySolutions, filteredSolution},
  (* keep the variables needed to solve (cause otherwise it just returns all of them, even if they are mid-rule *)
  originalVariables = variableCollector[predicates];
    (* case of negation. Check if you can't solve *)
  
  If[KeyExistsQ[predicates, "Negation"],
    Module[{negSolution = resolveSinglePredicate[predicates["Negation"], db, substitution]},
      Return[If[negSolution === {} || negSolution === $Failed, {substitution}, {}]]
    ]
  ];
  
  (* if bool, then it is regular *)
  If[KeyExistsQ[predicates, "bool"],
    Return[If[predicates["bool"] === "true", {substitution}, {}]]
  ];
  
  (* so it is a predicate, let's get the information from the database *)
  
  key = {predicates["head"], Length[predicates["arguments"]]};
  If[!KeyExistsQ[db, key], Return[{}]];
  
  (* if fact *)
  Do[
    Module[{renamedFact = variableRenamer[fact], unified},
      unified = unify[predicates["arguments"], renamedFact["arguments"], substitution];
      If[unified =!= $Failed,
        (* keep only the variables needed *)
        filteredSolution = KeySelect[unified, MemberQ[originalVariables, #]&];
	    If[originalVariables === {},
	       AppendTo[solutions, filteredSolution]
	    ]
        If[filteredSolution =!= <||>, 
          AppendTo[solutions, filteredSolution]
        ]
      ]
    ],
    {fact, db[key, "facts"]}
  ];
  
  (* if rule *)
  Do[
    Module[{renamedRule = variableRenamer[rule]},
      headUnified = unify[predicates["arguments"], renamedRule[[1]]["arguments"], substitution];
      If[headUnified =!= $Failed,
        (* continue to resolve the others *)
        bodySolutions = resolvePredicateList[renamedRule[[2]][[1]], db, headUnified];
        bodySolutions = Map[
          Function[sol, KeySelect[sol, MemberQ[originalVariables, #]&]],
          bodySolutions
        ];
        
        solutions = Join[solutions, Select[bodySolutions, Length[#] > 0 &]]
      ]
    ],
    {rule, db[key, "rules"]}
  ];
  
  If[solutions === {}, $Failed, solutions]
]

(* some formating functions, just for better printing *)
formatSolution[sol_] := StringRiffle[Map[# <> " = " <> formatTerm[sol[#]] &, Sort[Keys[sol]]], ", "];

formatTerm[term_] := Which[
  StringQ[term], term,
  AssociationQ[term] && KeyExistsQ[term, "Compound"], 
    term["Compound"] <> "(" <> StringRiffle[Map[formatTerm, term["Arguments"]], ","] <> ")",
  True, ToString[term]
];
