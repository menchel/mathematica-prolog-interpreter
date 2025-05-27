
(* just helper for a node *)
ClearAll[ASTNode];
ASTNode[type_, children___] := <|"type" -> type, "children" -> {children}|>;

(* some token helper functions *)
ClearAll[seeAnotherToken, getNextToken, matchToken];
SetAttributes[{seeAnotherToken, getNextToken, matchToken}, HoldFirst];

(* get the token, doesn't chnage the list *)
seeAnotherToken[tokens_] := If[Length[tokens] > 0, First[tokens], None];

(* get the token, chnage the list *)
getNextToken[tokens_] := If[Length[tokens] > 0,
  Module[{tok = First[tokens]}, tokens = Rest[tokens]; tok],
  None
];

(* try to match the expected token *)
matchToken[tokens_, matchTokened_] := Module[{tok = seeAnotherToken[tokens]},
  If[tok[[1]] === matchTokened,
    getNextToken[tokens],
    Message[matchToken::unmatchTokened, matchTokened, tok]; Abort[]
  ]
];
matchToken::unmatchTokened = "matchTokened `1`, but got `2`.";

(* the actual parser *)

ClearAll[
  parseProgram, parseClauseList, parseClause, parsePredicate,
  parsePredicateList, parseOrList, parseQuery, parseTermList,
  parseTerm, parseStructure, parseList, parseTail
];

(* main parse tree, start to check *)
parseProgram[tokens_] := Module[{allClauses, queryClause},
  If[seeAnotherToken[tokens][[1]] === "query",
    (* we saw a query, let's parse it personally *)
    queryClause = parseQuery[tokens];
    ASTNode["Program", queryClause],
    (* if not, then we started with some facts and/or rules and then a query *)
    allClauses = parseClauseList[tokens];
    queryClause = parseQuery[tokens];
    ASTNode["Program", allClauses, queryClause]
  ]
];

(* parses list of clauses *)
parseClauseList[tokens_] := Module[{allClauses = {}},
  (* we assume that it has at least one, otherwise we wouldn't have reached it *)
  AppendTo[allClauses, parseClause[tokens]];
  (* as long as we see Atom/Variable, it is possible *)
  While[MemberQ[{"Atom", "Variable"}, seeAnotherToken[tokens][[1]]],
    AppendTo[allClauses, parseClause[tokens]]
  ];
  ASTNode["ClauseList", Sequence @@ allClauses]
];

(* parses a single given clause *)
parseClause[tokens_] := Module[{start, body},
  (* get the predicate *)
  start = parsePredicate[tokens];
  (* we can either see a rule, or a fact *)
  If[seeAnotherToken[tokens][[1]] === "ColonDash",
    (* rule *)
    getNextToken[tokens];
    (* parse the list of conditions *)
    body = parseOrList[tokens];
    matchToken[tokens, "Dot"];
    ASTNode["Clause", start, body],
    (* fact *)
    matchToken[tokens, "Dot"];
    ASTNode["Fact", start]
  ]
];

(* parsing a query *)
parseQuery[tokens_] := Module[{res},
  matchToken[tokens, "query"];
  (* get the actual query *)
  res = parseOrList[tokens];
  (* we MUST end with . *)
  matchToken[tokens, "Dot"];
  ASTNode["Query", res]
];

(* parses a list (can have or, or not*)
parseOrList[tokens_] := Module[{ors = {}},
  (* begin with first list (or the only one if no ; ) *)
  AppendTo[ors, parsePredicateList[tokens]];
  (* if we see more ; , then it means another condition *)
  While[seeAnotherToken[tokens][[1]] === "or",
    getNextToken[tokens];
    (* parse the continuation *)
    AppendTo[ors, parsePredicateList[tokens]]
  ];
  (* differ between with or, or no ors *)
  If[Length[ors] == 1, First[ors], ASTNode["OrList", Sequence @@ ors]]
];

(* parses a list of predicates *)
parsePredicateList[tokens_] := Module[{preds = {}},
  (* parse the first *)
  AppendTo[preds, parsePredicate[tokens]];
  (* if there are more *)
  While[seeAnotherToken[tokens][[1]] === "Comma",
    getNextToken[tokens];
    (* continue to parse *)
    AppendTo[preds, parsePredicate[tokens]]
  ];
  (* differ between single and list *)
  If[Length[preds] == 1, First[preds], ASTNode["PredicateList", Sequence @@ preds]]
];

(* match a single predicate *)
parsePredicate[tokens_] := Module[{token, currentAtom, arguments},
  token = seeAnotherToken[tokens];
  (* waht can we see? *)
  Switch[token[[1]],
    (* a regular atom *)
    "Atom",
    currentAtom = getNextToken[tokens][[2]];
    (* check if it is a list of terms or not *)
    Print[tokens]
    If[seeAnotherToken[tokens][[1]] === "LParen",
      getNextToken[tokens];
      arguments = parseTermList[tokens];
      matchToken[tokens, "RParen"];
      ASTNode["Predicate", currentAtom, arguments]
      ASTNode["Predicate", currentAtom]
    ],
    (* note the case of negation *)
    "Negation",
    getNextToken[tokens];
    ASTNode["Not", parsePredicate[tokens]],
    (* we can get true/false as well *)
    "true" | "false",
    ASTNode["Boolean", getNextToken[tokens][[2]]],
    (* no match *)
    _, Message[parsePredicate::unmatchTokened, token]; Abort[]
  ]
];
parsePredicate::unmatchTokened = "UnmatchTokened token in predicate: `1`.";

(* matches a list of terms *)
parseTermList[tokens_] := Module[{terms = {}},
  (* parse the first term *)
  AppendTo[terms, parseTerm[tokens]];
  (* if there are more *)
  While[seeAnotherToken[tokens][[1]] === "Comma",
    getNextToken[tokens];
    (* parse the next one *)
    AppendTo[terms, parseTerm[tokens]]
  ];
  ASTNode["TermList", Sequence @@ terms]
];

(* parse a single term *)
parseTerm[tokens_] := Module[{tok = seeAnotherToken[tokens]},
  (* check what we got *)
  Switch[tok[[1]],
    "Number" | "Atom" | "Variable" | "placeHolder" | "String",
    ASTNode["Term", getNextToken[tokens][[2]]],
    (* case of list *)
    "LBracket", parseList[tokens],
    "Bar", ASTNode["Term", getNextToken[tokens][[2]]],
    _, Message[parseTerm::unmatchTokened, tok]; Abort[]
  ]
];
parseTerm::unmatchTokened = "UnmatchTokened token in term: `1`.";

(* parse a prolog list *)
parseList[tokens_] := Module[{start, tail},
  (* must start with [ *)
  matchToken[tokens, "LBracket"];
  If[seeAnotherToken[tokens][[1]] === "RBracket",
    (* empty list *)
    getNextToken[tokens];
    ASTNode["List", {}],
    (* non empty *)
    start = parseTerm[tokens];
    If[seeAnotherToken[tokens][[1]] === "Bar",
      (* place holder *)
      getNextToken[tokens];
      (* get next *)
      tail = parseTail[tokens];
      matchToken[tokens, "RBracket"];
      ASTNode["ListCons", start, tail],
      (* regular list *)
      tail = {};
      (* continue if needed *)
      While[seeAnotherToken[tokens][[1]] === "Comma",
        getNextToken[tokens];
        AppendTo[tail, parseTerm[tokens]]
      ];
      matchToken[tokens, "RBracket"];
      ASTNode["List", Sequence @@ Prepend[tail, start]]
    ]
  ]
];

(* parse the tail if a special list [ | ] *)
parseTail[tokens_] := Module[{token = seeAnotherToken[tokens]},
  Switch[token[[1]],
    "Variable", ASTNode["TailVar", getNextToken[tokens][[2]]],
    "LBracket", parseList[tokens],
    "Bar", ASTNode["TailBar", getNextToken[tokens][[2]]],
    _, Message[parseTail::unmatchTokened, token]; Abort[]
  ]
];
parseTail::unmatchTokened = "UnmatchTokened token in tail: `1`.";
