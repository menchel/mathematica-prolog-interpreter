tokens = tokenCreator[(* your code here *)];
(* just helper for a node *)
ClearAll[ASTNode];
ASTNode[type_, children___] := <|"type" -> type, "children" -> {children}|>;

(* token user *)
ClearAll[tokens];
tokens = tokenCreator["father(X,_,Y):-+father(X,Y),hello([X|XS]);true.?-father(a,b,c)."];

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
  parseTerm, parseStructure, parseList, parseTail
];

(* main parse tree, start to check *)
parseProgram[] := Module[{allClauses, singleQuery},
  If[seeAnotherToken[][[1]] === "query",
    (* we saw a query, let's parse it personally *)
    <|"Facts" -> {}, "Rules" -> {}, "Query" -> parseQuery[]|>,
    (* if not, then we started with some facts and/or rules and then a query *)
    allClauses = parseClauseList[];
    singleQuery = parseQuery[];
    Join[allClauses, <|"Query" -> singleQuery|>]
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
  While[MemberQ[{"Atom", "Variable"}, seeAnotherToken[][[1]]],
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
parseTerm[] := Module[{token = seeAnotherToken[]},
 (* check what we got *)
  Switch[token[[1]],
    "Number" | "Atom" | "Variable" | "placeHolder" | "String",
    getNextToken[][[2]],
    (* case of list *)
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
    "LBracket", parseList[],
    "Bar", getNextToken[][[2]],
    _, Message[parseTail::unmatchTokened, token]; Abort[]
  ]
];
