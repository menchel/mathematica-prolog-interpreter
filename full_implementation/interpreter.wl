tokenCreator[inputCode_String] := Module[{tokens},

  tokens = StringCases[
    inputCode,
    {
     (* go over all cases *)
      s : RegularExpression["true"] :> {"true",s},
      s : RegularExpression["false"] :> {"false",s},
      s : RegularExpression["_"] :> {"placeHolder",s},
      s : RegularExpression[";"] :> {"or",s},
      s : RegularExpression["\\s+"] :> Nothing,
      s : RegularExpression["[a-z][a-zA-Z0-9_]*"] :> {"Atom", s},
      s : RegularExpression["[A-Z_][a-zA-Z0-9_]*"] :> {"Variable", s},
      s : RegularExpression["\\d+(?:\\.\\d+)?"] :> {"Number", s},
      s : RegularExpression["'(?:[^']|'')*'"] :> {"String", s},

      (* special cases, mostly language built in signs *)
      "?-" :> {"query","?-"},
      ":-" :> {"ColonDash", ":-"},
      "." :> {"Dot", "."},
      "," :> {"Comma", ","},
      "(" :> {"LParen", "("},
      ")" :> {"RParen", ")"},
      "[" :> {"LBracket", "["},
      "]" :> {"RBracket", "]"},
      "|" :> {"Bar", "|"},
      "+" :> {"Negation","+"}
    }
  ];
  tokens
];
tokens = tokenCreator[(* your code here *)];
(* just helper for a node *)
ClearAll[ASTNode];
ASTNode[type_, children___] := <|"type" -> type, "children" -> {children}|>;

(* token user *)
ClearAll[tokens];
tokens = {};

(* some token helper functions *)
ClearAll[seeAnotherToken, getNextToken, matchToken];

(* get the token, doesn't change the list *)
seeAnotherToken[] := If[Length[tokens] > 0, First[tokens], None];

(* get the token, change the list *)
getNextToken[] := 
  If[Length[tokens] > 0,
    Module[{token = First[tokens]},
      tokens = Rest[tokens];
      token
    ],
    None
  ];

(* try to match the expected token *)
matchToken[expected_] := Module[{token = seeAnotherToken[]},
  If[token[[1]] === expected,
    getNextToken[],
    Message[matchToken::unmatchTokened, expected, token]; Abort[]
  ]
];
matchToken::unmatchTokened = "Expected `1`, but got `2`.";


(* the real parser :) *)

ClearAll[
  parseProgram, parseClauseList, parseClause, parsePredicate,
  parsePredicateList, parseOrList, parseQuery, parseTermList,
  parseTerm, parseList, parseTail
];

(* main parse tree, start to check *)
parseProgram[] := Module[
  {allClauses = <|"Facts" -> {}, "Rules" -> {}|>, allQueries = {}, token},
  token = seeAnotherToken[];
  
  (* If no tokens at all, return empty *)
  If[token === None,
    Return[<|"Facts" -> {}, "Rules" -> {}, "Query" -> {}|>]
  ];
  
  If[token[[1]] === "query",
    (* zero or more queries only, no clauses *)
    While[token =!= None && token[[1]] === "query",
      AppendTo[allQueries, parseQuery[]];
      token = seeAnotherToken[];
    ];
    <|"Facts" -> {}, "Rules" -> {}, "Query" -> allQueries|>,
    
    (* otherwise, parse clauses first *)
    
    allClauses = parseClauseList[];
    token = seeAnotherToken[];
    (* then zero or more queries *)
    While[token =!= None && token[[1]] === "query",
      AppendTo[allQueries, parseQuery[]];
      token = seeAnotherToken[];
    ];
    Join[allClauses, <|"Query" -> allQueries|>]
  ]
];




(* parses list of clauses *)
parseClauseList[] := Module[{allFacts = {}, allRules = {}, clause},
 (* we assume that it has at least one, otherwise we wouldn't have reached it *)
  clause = parseClause[];
  (* append according to if it is a rule or a fact *)
  Which[
    Head[clause] === Rule, AppendTo[allRules, clause],
    True, AppendTo[allFacts, clause]
  ];
  (* as long as we see Atom/Variable, it is possible *)
  While[seeAnotherToken[] =!= None && MemberQ[{"Atom", "Variable"}, seeAnotherToken[][[1]]],
    clause = parseClause[];
    Which[
      Head[clause] === Rule, AppendTo[allRules, clause],
      True, AppendTo[allFacts, clause]
    ];
  ];
  <|"Facts" -> allFacts, "Rules" -> allRules|>
];

(* parses a single given clause *)
parseClause[] := Module[{start, rest},
 (* get the predicate *)
  start = parsePredicate[];
  (* we can either see a rule, or a fact *)
  If[seeAnotherToken[][[1]] === "ColonDash",
   (* rule *)
    getNextToken[];
    (* parse the list of conditions *)
    rest = parseOrList[];
    matchToken["Dot"];
    start -> rest,
    (* fact *)
    matchToken["Dot"];
    start
  ]
];

(* parsing a query *)
parseQuery[] := Module[{body},
  matchToken["query"];
  (* get the actual query *)
  body = parseOrList[];
  (* we MUST end with . *)
  matchToken["Dot"];
  body
];

(* parses a list (can have or, or not*)
parseOrList[] := Module[{allors = {}},
 (* begin with first list (or the only one if no ; ) *)
  AppendTo[allors, parsePredicateList[]];
  (* if we see more ; , then it means another condition *)
  While[seeAnotherToken[][[1]] === "or",
    getNextToken[];
    (* parse the continuation *)
    AppendTo[allors, parsePredicateList[]];
  ];
  allors
];

(* parses a list of predicates *)
parsePredicateList[] := Module[{predicates = {}},
 (* parse the first *)
  AppendTo[predicates, parsePredicate[]];
  (* if there are more *)
  While[seeAnotherToken[][[1]] === "Comma",
    getNextToken[];
    (* continue to parse *)
    AppendTo[predicates, parsePredicate[]];
  ];
  predicates
];

(* match a single predicate *)
parsePredicate[] := Module[{token, head, arguments},
  token = seeAnotherToken[];
  (* what can we see? *)
  Switch[token[[1]],
    (* a regular atom *)
    "Atom",
    head = getNextToken[][[2]];
    (* check if it is a list of terms or not *)
    If[seeAnotherToken[][[1]] === "LParen",
      getNextToken[];
      arguments = parseTermList[];
      matchToken["RParen"];
      <|"head" -> head, "arguments" -> arguments|>,
      <|"head" -> head, "arguments" -> {}|>
    ],
    (* note the case of negation *)
    "Negation",
    getNextToken[];
    <|"Negation" -> parsePredicate[]|>,
    (* we can get true/false as well *)
    "true" | "false",
    <|"bool" -> getNextToken[][[2]]|>,
    (* no match *)
    _, Message[parsePredicate::unmatchTokened, token]; Abort[]
  ]
];

(* matches a list of terms *)
parseTermList[] := Module[{allTerms = {}},
 (* parse the first term *)
  AppendTo[allTerms, parseTerm[]];
  (* if there are more *)
  While[seeAnotherToken[][[1]] === "Comma",
    getNextToken[];
    (* parse the next one *)
    AppendTo[allTerms, parseTerm[]]
  ];
  allTerms
];

(* parse a single term *)
parseTerm[] := Module[{token = seeAnotherToken[], head, arguments},
  Switch[token[[1]],
  
    "Number" | "Variable" | "placeHolder" | "String",
    getNextToken[][[2]],

    "Atom",
    head = getNextToken[][[2]];
    If[seeAnotherToken[][[1]] === "LParen",
      getNextToken[]; (* consume LParen *)
      arguments = parseTermList[];
      matchToken["RParen"];
      <|"Compound" -> head, "Arguments" -> arguments|>,
      (* plain atom *)
      head
    ],

    "LBracket", parseList[],

    "Bar", getNextToken[][[2]],

    _, Message[parseTerm::unmatchTokened, token]; Abort[]
  ]
];



(* parse a prolog list *)
parseList[] := Module[{start, rest},
 (* must start with [ *)
  matchToken["LBracket"];
  If[seeAnotherToken[][[1]] === "RBracket",
   (* empty list *)
    getNextToken[];
    {},
    (* non empty *)
    start = parseTerm[];
    If[seeAnotherToken[][[1]] === "Bar",
     (* place holder *)
      getNextToken[];
      (* get next *)
      rest = parseTail[];
      matchToken["RBracket"];
      <|"ListHead" -> start, "Tail" -> rest|>,
     (* regular list *)
      rest = {};
      While[seeAnotherToken[][[1]] === "Comma",
       (* continue if needed *)
        getNextToken[];
        AppendTo[rest, parseTerm[]]
      ];
      matchToken["RBracket"];
      Prepend[rest, start]
    ]
  ]
];

(* parse the tail if a special list [ | ] *)
parseTail[] := Module[{token = seeAnotherToken[]},
  Switch[token[[1]],
    "Variable", getNextToken[][[2]],
    "placeHolder", getNextToken[][[2]],
    "LBracket", parseList[],
    _, Message[parseTail::unmatchTokened, token]; Abort[]
  ]
];
(* building a database for all the facts and rules *)
processClauses[clauses_Association] := Module[
  {facts = clauses["Facts"], rules = clauses["Rules"], dict = <||>},
  
  (* helper inner func *)
  getKey[currClause_] := Module[{head, arguments},
    Which[
      (* rule *)
      Head[currClause] === Rule,
        head = currClause[[1, "head"]];
        arguments = currClause[[1, "arguments"]],
      (* fact *)
      True,
        head = currClause["head"];
        arguments = currClause["arguments"]
    ];
    {head, Length[arguments]}
  ];
  
  (* add fact *)
  Do[
    Module[{key = getKey[fact]},
      If[KeyExistsQ[dict, key],
        AppendTo[dict[key, "facts"], fact],
        dict[key] = <|"facts" -> {fact}, "rules" -> {}|>
      ]
    ],
    {fact, facts}
  ];
  
  (* add rule *)
  Do[
    Module[{key = getKey[rule]},
      If[KeyExistsQ[dict, key],
        AppendTo[dict[key, "rules"], rule],
        dict[key] = <|"facts" -> {}, "rules" -> {rule}|>
      ]
    ],
    {rule, rules}
  ];
  
  dict
];
(* renamer *)
ClearAll[variableRenamer, variableReplacer, variableCollector, uniqueVar];

(* create unique names each time *)
uniqueVar[] := Module[{count = 0}, 
  Function[Symbol["TT" <> ToString[count++]]]
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
