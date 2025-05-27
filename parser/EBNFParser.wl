(* ==== AST Constructor ==== *)
ClearAll[ASTNode];
ASTNode[type_, children___] := <|"type" -> type, "children" -> {children}|>;

(* ==== Token Stream Utilities ==== *)
ClearAll[seeAnotherToken, getNextToken, matchToken];
SetAttributes[{seeAnotherToken, getNextToken, matchToken}, HoldFirst];

seeAnotherToken[tokens_] := If[Length[tokens] > 0, First[tokens], None];

getNextToken[tokens_] := If[Length[tokens] > 0,
  Module[{tok = First[tokens]}, tokens = Rest[tokens]; tok],
  None
];

matchToken[tokens_, matchTokened_] := Module[{tok = seeAnotherToken[tokens]},
  If[tok[[1]] === matchTokened,
    getNextToken[tokens],
    Message[matchToken::unmatchTokened, matchTokened, tok]; Abort[]
  ]
];
matchToken::unmatchTokened = "matchTokened `1`, but got `2`.";

(* ==== Parser Rules ==== *)

ClearAll[
  parseProgram, parseClauseList, parseClause, parsePredicate,
  parsePredicateList, parseOrList, parseQuery, parseTermList,
  parseTerm, parseStructure, parseList, parseTail
];

parseProgram[tokens_] := Module[{clauses, query},
  If[seeAnotherToken[tokens][[1]] === "query",
    query = parseQuery[tokens];
    ASTNode["Program", query],
    clauses = parseClauseList[tokens];
    query = parseQuery[tokens];
    ASTNode["Program", clauses, query]
  ]
];

parseClauseList[tokens_] := Module[{clauses = {}},
  AppendTo[clauses, parseClause[tokens]];
  While[MemberQ[{"Atom", "Variable"}, seeAnotherToken[tokens][[1]]],
    AppendTo[clauses, parseClause[tokens]]
  ];
  ASTNode["ClauseList", Sequence @@ clauses]
];

parseClause[tokens_] := Module[{head, body},
  head = parsePredicate[tokens];
  If[seeAnotherToken[tokens][[1]] === "ColonDash",
    getNextToken[tokens];
    body = parseOrList[tokens];
    matchToken[tokens, "Dot"];
    ASTNode["Clause", head, body],
    matchToken[tokens, "Dot"];
    ASTNode["Fact", head]
  ]
];

parseQuery[tokens_] := Module[{q},
  matchToken[tokens, "query"];
  q = parseOrList[tokens];
  matchToken[tokens, "Dot"];
  ASTNode["Query", q]
];

parseOrList[tokens_] := Module[{ors = {}},
  AppendTo[ors, parsePredicateList[tokens]];
  While[seeAnotherToken[tokens][[1]] === "or",
    getNextToken[tokens]; (* consume ; *)
    AppendTo[ors, parsePredicateList[tokens]]
  ];
  If[Length[ors] == 1, First[ors], ASTNode["OrList", Sequence @@ ors]]
];

parsePredicateList[tokens_] := Module[{preds = {}},
  AppendTo[preds, parsePredicate[tokens]];
  While[seeAnotherToken[tokens][[1]] === "Comma",
    getNextToken[tokens];
    AppendTo[preds, parsePredicate[tokens]]
  ];
  If[Length[preds] == 1, First[preds], ASTNode["PredicateList", Sequence @@ preds]]
];

parsePredicate[tokens_] := Module[{tok, atom, args},
  tok = seeAnotherToken[tokens];
  Switch[tok[[1]],
    "Atom",
    atom = getNextToken[tokens][[2]];
    If[seeAnotherToken[tokens][[1]] === "RParen",
      getNextToken[tokens];
      args = parseTermList[tokens];
      matchToken[tokens, "LParen"];
      ASTNode["Predicate", atom, args],
      ASTNode["Predicate", atom]
    ],
    "Negation",
    getNextToken[tokens];
    ASTNode["Not", parsePredicate[tokens]],
    "true" | "false",
    ASTNode["Boolean", getNextToken[tokens][[2]]],
    _, Message[parsePredicate::unmatchTokened, tok]; Abort[]
  ]
];
parsePredicate::unmatchTokened = "UnmatchTokened token in predicate: `1`.";

parseTermList[tokens_] := Module[{terms = {}},
  AppendTo[terms, parseTerm[tokens]];
  While[seeAnotherToken[tokens][[1]] === "Comma",
    getNextToken[tokens];
    AppendTo[terms, parseTerm[tokens]]
  ];
  ASTNode["TermList", Sequence @@ terms]
];

parseTerm[tokens_] := Module[{tok = seeAnotherToken[tokens]},
  Switch[tok[[1]],
    "Number" | "Atom" | "Variable" | "placeHolder" | "String",
    ASTNode["Term", getNextToken[tokens][[2]]],
    "LBracket", parseList[tokens],
    "Bar", ASTNode["Term", getNextToken[tokens][[2]]],
    _, Message[parseTerm::unmatchTokened, tok]; Abort[]
  ]
];
parseTerm::unmatchTokened = "UnmatchTokened token in term: `1`.";

parseList[tokens_] := Module[{head, tail},
  matchToken[tokens, "LBracket"];
  If[seeAnotherToken[tokens][[1]] === "RBracket",
    getNextToken[tokens];
    ASTNode["List", {}],
    head = parseTerm[tokens];
    If[seeAnotherToken[tokens][[1]] === "Bar",
      getNextToken[tokens];
      tail = parseTail[tokens];
      matchToken[tokens, "RBracket"];
      ASTNode["ListCons", head, tail],
      (* else regular list *)
      tail = {};
      While[seeAnotherToken[tokens][[1]] === "Comma",
        getNextToken[tokens];
        AppendTo[tail, parseTerm[tokens]]
      ];
      matchToken[tokens, "RBracket"];
      ASTNode["List", Sequence @@ Prepend[tail, head]]
    ]
  ]
];

parseTail[tokens_] := Module[{tok = seeAnotherToken[tokens]},
  Switch[tok[[1]],
    "Variable", ASTNode["TailVar", getNextToken[tokens][[2]]],
    "LBracket", parseList[tokens],
    "Bar", ASTNode["TailBar", getNextToken[tokens][[2]]],
    _, Message[parseTail::unmatchTokened, tok]; Abort[]
  ]
];
parseTail::unmatchTokened = "UnmatchTokened token in tail: `1`.";
