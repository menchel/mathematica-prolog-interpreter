(* ============ query resolver ============= *)
(*
	The query resolver gets a queries from the parser
	, and tries solve them using the information in the dictionary
*)
(* ==================================== *)
(* binder *)
ClearAll[resolveStruct]
resolveStruct[expr_, mapping_] := 
  Module[{r},
    r[x_] := Which[
      (* If x is a symbol and exists in mapping, resolve it *)
      Head[x] === Symbol && KeyExistsQ[mapping, x], r[mapping[x]],
      
      (* Compound term *)
      AssociationQ[x] && KeyExistsQ[x, "Compound"], 
        <| "Compound" -> x["Compound"], 
           "Arguments" -> r /@ x["Arguments"] |>,
      
      (* List or other structures *)
      ListQ[x], r /@ x,
      
      (* Otherwise return unchanged *)
      True, x
    ];
    r[expr]
  ];


(* query resolver *)
ClearAll[resolveQuery];

(* main resolve *)
resolveQuery[queries_, db_] := Module[{results = {}},
  uniqueVarGenerator = uniqueVar[]; (* unique number for each *)
  Do[ (* take care of all query *)
    Module[{solutions = resolveSingleQuery[q[[1]], db]}, (* a single query *)
      (*If[solutions === {} || solutions === $Failed,
        Print["  No solutions found"],
        (* print all of the solutions *)
        Do[
          Print["  Solution " <> ToString[solNum] <> ": " <> formatSolution[sol]],
          {sol, solutions}, {solNum, Length[solutions]}
        ]
      ];*)
      AppendTo[results, solutions]
    ];
    ,{q, queries}
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
  If[predicateList === {}, Return[{substitution}]]; (* if no predicate to solve *)
  Module[{firstSolutions = resolveSinglePredicate[firstPredicate, db, substitution]}, (* we have predicates *)
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


filterCompatibleRules[first_Association, rules_List] := 
  Select[rules, rule |-> 
    AllTrue[
      Keys[first],
      key |-> !KeyExistsQ[rule, key] || SameQ[rule[key], first[key]]
    ]
  ];
  

(* resolve a single predicates *)
resolveSinglePredicate[predicates_, db_, substitution_] := Module[
  {solutions = {}, originalVariables, headUnified, bodySolutions, filteredSolution},
  originalVariables = variableCollector[predicates]; (* keep the variables needed to solve (cause otherwise it just returns all of them, even if they are mid-rule *)
  If[KeyExistsQ[predicates, "Negation"],     (* case of negation. Check if you can't solve *)
    Module[{negSolution = resolveSinglePredicate[predicates["Negation"], db, substitution]},
      Return[If[negSolution === {} || negSolution === $Failed, {substitution}, {}]]
    ]
  ];
  
  If[KeyExistsQ[predicates, "bool"], (* if bool, then it is regular *)
    Return[If[predicates["bool"] === "true", {substitution}, {}]]
  ];
  
  key = {predicates["head"], Length[predicates["arguments"]]}; (* so it is a predicate, let's get the information from the database *)
  If[!KeyExistsQ[db, key], Return[{}]];
  Do[ (* if fact *)
    Module[{renamedFact = variableRenamer[fact], unified},
      unified = unify[predicates["arguments"], renamedFact["arguments"], substitution];
      If[unified =!= $Failed,
        unified = unified //.unified;
        filteredSolution = KeySelect[unified, MemberQ[originalVariables, #]&]; (* keep only the variables needed *)
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
  
  Do[ (* if rule *)
    Module[{renamedRule = variableRenamer[rule]},
      headUnified = unify[predicates["arguments"], renamedRule[[1]]["arguments"], substitution];
      If[headUnified =!= $Failed,
        bodySolutions = resolvePredicateList[First[renamedRule[[2]]], db, headUnified]; (* continue to resolve the others *)
        If[ bodySolutions=!={},
        headUnified = headUnified //. headUnified;
        (* need to only select the ones who are good! *)
        bodySolutions = filterCompatibleRules[headUnified,bodySolutions];
        If[bodySolutions=!={},
	        listInforOfHead = headUnified //. bodySolutions; (* for recursive solutions! *)
	        listInforOfHead = Map[
	          Function[sol, KeySelect[sol, MemberQ[originalVariables, #]&]],
	          listInforOfHead
	        ];
	        solutions = Join[
					    solutions, 
					    Select[listInforOfHead, 
					        Not[MatchQ[#, <||>]] || True &
					    ]
					]
				]
		]
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
