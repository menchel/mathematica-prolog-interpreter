BeginPackage["MyPrologInterpreter`"]

(* Exported symbols *)
Interpret::usage = "interprets the code in in.pl, and puts the output in out.pl";
PrologDepth::usage = "set a depth for the interpreter";

Begin["`Private`"]

(* =========== token creator =========== *)
(*
	The token creator gets the string input from the user
	, and creates tokens from it for future use
*)
(* ==================================== *)
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

(* ============== parser =============== *)
(*
	The parser creator gets the tokens from the token creator
	, and creates parsed AST node that can be used later
*)
(* ==================================== *)

(* just helper for a node *)
ASTNode[type_, children___] := <|"type" -> type, "children" -> {children}|>;

(* token user *)
tokens = {};

(* some token helper functions *)

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

(* main parse tree, start to check *)
parseProgram[] := Module[
  {allClauses = <|"Facts" -> {}, "Rules" -> {}|>, allQueries = {}, token},
  token = seeAnotherToken[];
  
  If[token === None,
    Return[<|"Facts" -> {}, "Rules" -> {}, "Query" -> {}|>]
  ];(* If no tokens at all, return empty *)
  
  If[token[[1]] === "query",
    While[token =!= None && token[[1]] === "query",
      AppendTo[allQueries, parseQuery[]];
      token = seeAnotherToken[];
    ]; (* zero or more queries only, no clauses *)
    <|"Facts" -> {}, "Rules" -> {}, "Query" -> allQueries|>,
    
    allClauses = parseClauseList[]; (* otherwise, parse clauses first *)
    token = seeAnotherToken[];
    While[token =!= None && token[[1]] === "query",
      AppendTo[allQueries, parseQuery[]];
      token = seeAnotherToken[];
    ]; (* then zero or more queries *)
    Join[allClauses, <|"Query" -> allQueries|>]
  ]
];




(* parses list of clauses *)
parseClauseList[] := Module[{allFacts = {}, allRules = {}, clause},
  clause = parseClause[]; (* we assume that it has at least one, otherwise we wouldn't have reached it *)
  Which[
    Head[clause] === Rule, AppendTo[allRules, clause],
    True, AppendTo[allFacts, clause]
  ]; (* append according to if it is a rule or a fact *)
  While[seeAnotherToken[] =!= None && MemberQ[{"Atom", "Variable"}, seeAnotherToken[][[1]]],
    clause = parseClause[];
    Which[
      Head[clause] === Rule, AppendTo[allRules, clause],
      True, AppendTo[allFacts, clause]
    ];
  ]; (* as long as we see Atom/Variable, it is possible *)
  <|"Facts" -> allFacts, "Rules" -> allRules|>
];

(* parses a single given clause *)
parseClause[] := Module[{start, rest},
  start = parsePredicate[]; (* get the predicate *)
  If[seeAnotherToken[][[1]] === "ColonDash", (* we can either see a rule, or a fact *)
    getNextToken[]; (* rule *)
    rest = parseOrList[]; (* parse the list of conditions *)
    matchToken["Dot"];
    start -> rest,
    matchToken["Dot"]; (* fact *)
    start
  ]
];

(* parsing a query *)
parseQuery[] := Module[{body},
  matchToken["query"];
  body = parseOrList[]; (* get the actual query *)
  matchToken["Dot"]; (* we MUST end with . *)
  body
];

(* parses a list (can have or, or not*)
parseOrList[] := Module[{allors = {}},
  AppendTo[allors, parsePredicateList[]]; (* begin with first list (or the only one if no ; ) *)
  While[seeAnotherToken[][[1]] === "or", (* if we see more ; , then it means another condition *)
    getNextToken[];
    AppendTo[allors, parsePredicateList[]]; (* parse the continuation *)
  ];
  allors
];

(* parses a list of predicates *)
parsePredicateList[] := Module[{predicates = {}},
  AppendTo[predicates, parsePredicate[]]; (* parse the first *)
  While[seeAnotherToken[][[1]] === "Comma", (* if there are more *)
    getNextToken[];
    AppendTo[predicates, parsePredicate[]]; (* continue to parse *)
  ];
  predicates
];

(* match a single predicate *)
parsePredicate[] := Module[{token, head, arguments},
  token = seeAnotherToken[];
  Switch[token[[1]], (* what can we see? *)
    "Atom", (* a regular atom *)
    head = getNextToken[][[2]];
    If[seeAnotherToken[][[1]] === "LParen", (* check if it is a list of terms or not *)
      getNextToken[];
      arguments = parseTermList[];
      matchToken["RParen"];
      <|"head" -> head, "arguments" -> arguments|>,
      <|"head" -> head, "arguments" -> {}|>
    ],
    "Negation", (* note the case of negation *)
    getNextToken[];
    <|"Negation" -> parsePredicate[]|>,
    "true" | "false",     (* we can get true/false as well *)
    <|"bool" -> getNextToken[][[2]]|>,
    _, Message[parsePredicate::unmatchTokened, token]; Abort[]  (* no match *)
  ]
];

(* matches a list of terms *)
parseTermList[] := Module[{allTerms = {}},
  AppendTo[allTerms, parseTerm[]]; (* parse the first term *)
  While[seeAnotherToken[][[1]] === "Comma", (* if there are more *)
    getNextToken[];
    AppendTo[allTerms, parseTerm[]] (* parse the next one *)
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
      head (* plain atom *)
    ],

    "LBracket", parseList[],

    "Bar", getNextToken[][[2]],

    _, Message[parseTerm::unmatchTokened, token]; Abort[]
  ]
];



(* parse a prolog list *)
parseList[] := Module[{start, rest},
  matchToken["LBracket"]; (* must start with [ *)
  If[seeAnotherToken[][[1]] === "RBracket",
    getNextToken[]; (* empty list *)
    {},
    start = parseTerm[];  (* non empty *)
    If[seeAnotherToken[][[1]] === "Bar",
      getNextToken[];      (* place holder *)
      rest = parseTail[];       (* get next *)
      matchToken["RBracket"];
      <|"ListHead" -> start, "Tail" -> rest|>,
      rest = {};      (* regular list *)
      While[seeAnotherToken[][[1]] === "Comma",
        getNextToken[];        (* continue if needed *)
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

(* ============ dictionary ============= *)
(*
	The dictionary creator gets the parsed facts and rules from the parser
	, and creates a dictionary of [functor][arity] for future use
*)
(* ==================================== *)

(* building a database for all the facts and rules *)
processClauses[clauses_Association] := Module[
  {facts = clauses["Facts"], rules = clauses["Rules"], dict = <||>},
  
  (* helper inner func *)
  getKey[currClause_] := Module[{head, arguments},
    Which[
      Head[currClause] === Rule, (* rule *)
        head = currClause[[1, "head"]];
        arguments = currClause[[1, "arguments"]],
      True,    (* fact *)
        head = currClause["head"];
        arguments = currClause["arguments"]
    ];
    {head, Length[arguments]}
  ];
  

  Do[   (* add fact *)
    Module[{key = getKey[fact]},
      If[KeyExistsQ[dict, key],
        AppendTo[dict[key, "facts"], fact],
        dict[key] = <|"facts" -> {fact}, "rules" -> {}|>
      ]
    ],
    {fact, facts}
  ];
  
  Do[ (* add rule *)
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

(* ============ renamer ============= *)
(*
	The renamer gets a parsed fact or rule from the dictionary
	, and creates a new version with fresh variables
*)
(* ==================================== *)

(* renamer *)

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


(* ============ unify ============= *)
(*
	The unify gets a query and a term
	, and tries to compare them, infering connections of variables on the way
*)
(* ==================================== *)

(* unify *)

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


(* ============ query resolver ============= *)
(*
	The query resolver gets a queries from the parser
	, and tries solve them using the information in the dictionary
*)
(* ==================================== *)
(* binder *)
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
PrologDepth = 10;

currentDepth = 0;

(* main resolve *)
resolveQuery[queries_, db_] := Module[{results = {}},
  uniqueVarGenerator = uniqueVar[]; (* unique number for each *)
  currentDepth = 0;
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
  currentDepth = currentDepth + 1;
  If[PrologDepth < currentDepth,Return[{}]];
  If[KeyExistsQ[predicates, "Negation"],     (* case of negation. Check if you can't solve *)
    Module[{negSolution = resolveSinglePredicate[predicates["Negation"], db, substitution]},
      Return[If[negSolution === {} || negSolution === $Failed, 
      currentDepth = currentDepth - 1;
      {substitution}, {}]]
    ]
  ];
  
  If[KeyExistsQ[predicates, "bool"], (* if bool, then it is regular *)
    Return[If[predicates["bool"] === "true", 
    currentDepth = currentDepth - 1;
    {substitution}, {}]]
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
	        listInforOfHead = DeleteDuplicates[listInforOfHead];
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
  currentDepth = currentDepth - 1;
  If[solutions === {}, $Failed, DeleteDuplicates[solutions]]
]


(* ============ input/output manager ============= *)
(*
	The input/output manager recieves the input from in.pl
	, and put the answers of the interpreter in out.pl
*)
(* ==================================== *)
(* formating to print more nice true and false *)
printElement[element_] := 
  If[element === {}, 
   "false", 
   If[element === {<||>} || element === {Association[]}, 
    "true",
    ToString[element]]];
    
End[]

PrologDepth = 10;

(* more formating *)
isRecursiveListHead[term_] := Module[{head = term},
  While[AssociationQ[head] && KeyExistsQ[head, "ListHead"],
    head = head["Tail"];
  ];
  head === {} || head === Association[] || ListQ[head]
]


formatList[list_] := Module[{elements = {}, head = list},
  While[AssociationQ[head] && KeyExistsQ[head, "ListHead"],
    AppendTo[elements, formatTerm[head["ListHead"]]];
    head = head["Tail"];
  ];
  
  Which[ (* if tail is a list, then faltten it *)
    head === {} || head === Association[],
      StringRiffle[elements, ", "],
    ListQ[head],
      StringRiffle[Join[elements, formatTerm /@ head], ", "],
    True,
      StringRiffle[elements, ", "] <> " | " <> formatTerm[head]
  ]
]


formatTerm[termVal_] := Which[
  StringQ[termVal], termVal, (* normal *)
  AssociationQ[termVal] && KeyExistsQ[termVal, "Compound"], (* compund *)
    termVal["Compound"] <> "(" <> StringRiffle[formatTerm /@ termVal["Arguments"], ", "] <> ")",
  AssociationQ[termVal] && KeyExistsQ[termVal, "ListHead"] && isRecursiveListHead[termVal], (* list, but with recursive head *)
    "[" <> formatList[termVal] <> "]",
  AssociationQ[termVal] && KeyExistsQ[termVal, "ListHead"], (* other list *)
    "[" <> formatList[termVal] <> "]",
  ListQ[termVal], "[" <> StringRiffle[formatTerm /@ termVal, ", "] <> "]",
  True, ToString[termVal]
]

(* format <|X -> a|> into "X = a" *)
formatSubstitution[sub_Association] := 
  StringRiffle[KeyValueMap[#1 <> " = " <> formatTerm[#2] &, sub], ", "]

(* fromat X = a ; X = b. *)
formatAnswer[{}] := "false."
formatAnswer[{a_Association}] /; a === <||> := "true."
formatAnswer[{a_Association}] /; a === Association[] := "true."
formatAnswer[True] := "true."
formatAnswer[False] := "false."
formatAnswer[subs_List] := StringRiffle[formatSubstitution /@ subs, " ; "] <> "."
formatAnswer[other_] := ToString[other] (* else catch *)


Interpret[] := Module[
  {progText, parsed, db, solutions, output},
  progText = Import[FileNameJoin[{NotebookDirectory[], "in.pl"}], "Text"];
  tokens = tokenCreator[progText];
  parsed = parseProgram[];
  db = processClauses[parsed];
  solutions = resolveQuery[parsed["Query"], db];
  output = formatAnswer /@ solutions;
  Export[FileNameJoin[{NotebookDirectory[], "out.pl"}], StringRiffle[output, "\n"], "Text"];
]

EndPackage[]

PrologDepth:=15

Interpret[];
