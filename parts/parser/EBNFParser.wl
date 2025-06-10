(* ============== parser =============== *)
(*
	The parser creator gets the tokens from the token creator
	, and creates parsed AST node that can be used later
*)
(* ==================================== *)

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
