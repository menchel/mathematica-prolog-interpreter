(* just helper for a node *)
ClearAll[ASTNode];
ASTNode[type_, children___] := <|"type" -> type, "children" -> {children}|>;

(* token user *)
ClearAll[tokens];
tokens = tokenCreator[(* your prolog code *)];

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

(* the actual parser *)

ClearAll[
  parseProgram, parseClauseList, parseClause, parsePredicate,
  parsePredicateList, parseOrList, parseQuery, parseTermList,
  parseTerm, parseStructure, parseList, parseTail
];

(* main parse tree, start to check *)
parseProgram[] := Module[{allClauses, queryClause},
  If[seeAnotherToken[][[1]] === "query",
    (* we saw a query, let's parse it personally *)
    queryClause = parseQuery[];
    ASTNode["Program", queryClause],
    (* if not, then we started with some facts and/or rules and then a query *)
    allClauses = parseClauseList[];
    queryClause = parseQuery[];
    ASTNode["Program", allClauses, queryClause]
  ]
];

(* parses list of clauses *)
parseClauseList[] := Module[{allClauses = {}},
  (* we assume that it has at least one, otherwise we wouldn't have reached it *)
  AppendTo[allClauses, parseClause[]];
  (* as long as we see Atom/Variable, it is possible *)
  While[MemberQ[{"Atom", "Variable"}, seeAnotherToken[][[1]]],
    AppendTo[allClauses, parseClause[]]
  ];
  ASTNode["ClauseList", Sequence @@ allClauses]
];

(* parses a single given clause *)
parseClause[] := Module[{start, body},
  (* get the predicate *)
  start = parsePredicate[];
  (* we can either see a rule, or a fact *)
  If[seeAnotherToken[][[1]] === "ColonDash",
    (* rule *)
    getNextToken[];
    (* parse the list of conditions *)
    body = parseOrList[];
    matchToken["Dot"];
    ASTNode["Clause", start, body],
    (* fact *)
    matchToken["Dot"];
    ASTNode["Fact", start]
  ]
];

(* parsing a query *)
parseQuery[] := Module[{res},
  matchToken["query"];
  (* get the actual query *)
  res = parseOrList[];
  (* we MUST end with . *)
  matchToken["Dot"];
  ASTNode["Query", res]
];

(* parses a list (can have or, or not*)
parseOrList[] := Module[{ors = {}},
  (* begin with first list (or the only one if no ; ) *)
  AppendTo[ors, parsePredicateList[]];
  (* if we see more ; , then it means another condition *)
  While[seeAnotherToken[][[1]] === "or",
    getNextToken[];
    (* parse the continuation *)
    AppendTo[ors, parsePredicateList[]]
  ];
  (* differ between with or, or no ors *)
  If[Length[ors] == 1, First[ors], ASTNode["OrList", Sequence @@ ors]]
];

(* parses a list of predicates *)
parsePredicateList[] := Module[{preds = {}},
  (* parse the first *)
  AppendTo[preds, parsePredicate[]];
  (* if there are more *)
  While[seeAnotherToken[][[1]] === "Comma",
    getNextToken[];
    (* continue to parse *)
    AppendTo[preds, parsePredicate[]]
  ];
  (* differ between single and list *)
  If[Length[preds] == 1, First[preds], ASTNode["PredicateList", Sequence @@ preds]]
];

(* match a single predicate *)
parsePredicate[] := Module[{token, currentAtom, arguments},
  token = seeAnotherToken[];
  (* waht can we see? *)
  Switch[token[[1]],
    (* a regular atom *)
    "Atom",
    currentAtom = getNextToken[][[2]];
    (* check if it is a list of terms or not *)
    If[seeAnotherToken[][[1]] === "LParen",
      getNextToken[];
      arguments = parseTermList[];
      matchToken["RParen"];
      ASTNode["Predicate", currentAtom, arguments],
      ASTNode["Predicate", currentAtom]
    ],
    (* note the case of negation *)
    "Negation",
    getNextToken[];
    ASTNode["Not", parsePredicate[]],
    (* we can get true/false as well *)
    "true" | "false",
    ASTNode["Boolean", getNextToken[][[2]]],
    (* no match *)
    _, Message[parsePredicate::unmatchTokened, token]; Abort[]
  ]
];
parsePredicate::unmatchTokened = "UnmatchTokened token in predicate: `1`.";

(* matches a list of terms *)
parseTermList[] := Module[{terms = {}},
  (* parse the first term *)
  AppendTo[terms, parseTerm[]];
  (* if there are more *)
  While[seeAnotherToken[][[1]] === "Comma",
    getNextToken[];
    (* parse the next one *)
    AppendTo[terms, parseTerm[]]
  ];
  ASTNode["TermList", Sequence @@ terms]
];

(* parse a single term *)
parseTerm[] := Module[{tok = seeAnotherToken[]},
  (* check what we got *)
  Switch[tok[[1]],
    "Number" | "Atom" | "Variable" | "placeHolder" | "String",
    ASTNode["Term", getNextToken[][[2]]],
    (* case of list *)
    "LBracket", parseList[],
    "Bar", ASTNode["Term", getNextToken[][[2]]],
    _, Message[parseTerm::unmatchTokened, tok]; Abort[]
  ]
];
parseTerm::unmatchTokened = "UnmatchTokened token in term: `1`.";

(* parse a prolog list *)
parseList[] := Module[{start, tail},
  (* must start with [ *)
  matchToken["LBracket"];
  If[seeAnotherToken[][[1]] === "RBracket",
    (* empty list *)
    getNextToken[];
    ASTNode["List", {}],
    (* non empty *)
    start = parseTerm[];
    If[seeAnotherToken[][[1]] === "Bar",
      (* place holder *)
      getNextToken[];
      (* get next *)
      tail = parseTail[];
      matchToken["RBracket"];
      ASTNode["ListCons", start, tail],
      (* regular list *)
      tail = {};
      (* continue if needed *)
      While[seeAnotherToken[][[1]] === "Comma",
        getNextToken[];
        AppendTo[tail, parseTerm[]]
      ];
      matchToken["RBracket"];
      ASTNode["List", Sequence @@ Prepend[tail, start]]
    ]
  ]
];

(* parse the tail if a special list [ | ] *)
parseTail[] := Module[{token = seeAnotherToken[]},
  Switch[token[[1]],
    "Variable", ASTNode["TailVar", getNextToken[][[2]]],
    "LBracket", parseList[],
    "Bar", ASTNode["TailBar", getNextToken[][[2]]],
    _, Message[parseTail::unmatchTokened, token]; Abort[]
  ]
];
parseTail::unmatchTokened = "UnmatchTokened token in tail: `1`.";
