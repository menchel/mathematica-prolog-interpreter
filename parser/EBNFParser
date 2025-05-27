(* ==== AST Constructor ==== *)
ClearAll[ASTNode];
ASTNode[type_, children___] := <|"type" -> type, "children" -> {children}|>;

(* ==== Token Stream Utilities ==== *)
ClearAll[peekToken, nextToken, expect];
SetAttributes[{peekToken, nextToken, expect}, HoldFirst];

peekToken[tokens_] := If[Length[tokens] > 0, First[tokens], None];

nextToken[tokens_] := If[Length[tokens] > 0,
  Module[{tok = First[tokens]}, tokens = Rest[tokens]; tok],
  None
];

expect[tokens_, expected_] := Module[{tok = peekToken[tokens]},
  If[tok[[1]] === expected,
    nextToken[tokens],
    Message[expect::unexpected, expected, tok]; Abort[]
  ]
];
expect::unexpected = "Expected `1`, but got `2`.";

(* ==== Parser Rules ==== *)

ClearAll[
  parseProgram, parseClauseList, parseClause, parsePredicate,
  parsePredicateList, parseOrList, parseQuery, parseTermList,
  parseTerm, parseStructure, parseList, parseTail
];

parseProgram[tokens_] := Module[{clauses, query},
  If[peekToken[tokens][[1]] === "query",
    query = parseQuery[tokens];
    ASTNode["Program", query],
    clauses = parseClauseList[tokens];
    query = parseQuery[tokens];
    ASTNode["Program", clauses, query]
  ]
];

parseClauseList[tokens_] := Module[{clauses = {}},
  AppendTo[clauses, parseClause[tokens]];
  While[MemberQ[{"Atom", "Variable"}, peekToken[tokens][[1]]],
    AppendTo[clauses, parseClause[tokens]]
  ];
  ASTNode["ClauseList", Sequence @@ clauses]
];

parseClause[tokens_] := Module[{head, body},
  head = parsePredicate[tokens];
  If[peekToken[tokens][[1]] === "ColonDash",
    nextToken[tokens];
    body = parseOrList[tokens];
    expect[tokens, "Dot"];
    ASTNode["Clause", head, body],
    expect[tokens, "Dot"];
    ASTNode["Fact", head]
  ]
];

parseQuery[tokens_] := Module[{q},
  expect[tokens, "query"];
  q = parseOrList[tokens];
  expect[tokens, "Dot"];
  ASTNode["Query", q]
];

parseOrList[tokens_] := Module[{ors = {}},
  AppendTo[ors, parsePredicateList[tokens]];
  While[peekToken[tokens][[1]] === "or",
    nextToken[tokens]; (* consume ; *)
    AppendTo[ors, parsePredicateList[tokens]]
  ];
  If[Length[ors] == 1, First[ors], ASTNode["OrList", Sequence @@ ors]]
];

parsePredicateList[tokens_] := Module[{preds = {}},
  AppendTo[preds, parsePredicate[tokens]];
  While[peekToken[tokens][[1]] === "Comma",
    nextToken[tokens];
    AppendTo[preds, parsePredicate[tokens]]
  ];
  If[Length[preds] == 1, First[preds], ASTNode["PredicateList", Sequence @@ preds]]
];

parsePredicate[tokens_] := Module[{tok, atom, args},
  tok = peekToken[tokens];
  Switch[tok[[1]],
    "Atom",
    atom = nextToken[tokens][[2]];
    If[peekToken[tokens][[1]] === "RParen",
      nextToken[tokens];
      args = parseTermList[tokens];
      expect[tokens, "LParen"];
      ASTNode["Predicate", atom, args],
      ASTNode["Predicate", atom]
    ],
    "Negation",
    nextToken[tokens];
    ASTNode["Not", parsePredicate[tokens]],
    "true" | "false",
    ASTNode["Boolean", nextToken[tokens][[2]]],
    _, Message[parsePredicate::unexpected, tok]; Abort[]
  ]
];
parsePredicate::unexpected = "Unexpected token in predicate: `1`.";

parseTermList[tokens_] := Module[{terms = {}},
  AppendTo[terms, parseTerm[tokens]];
  While[peekToken[tokens][[1]] === "Comma",
    nextToken[tokens];
    AppendTo[terms, parseTerm[tokens]]
  ];
  ASTNode["TermList", Sequence @@ terms]
];

parseTerm[tokens_] := Module[{tok = peekToken[tokens]},
  Switch[tok[[1]],
    "Number" | "Atom" | "Variable" | "placeHolder" | "String",
    ASTNode["Term", nextToken[tokens][[2]]],
    "LBracket", parseList[tokens],
    "Bar", ASTNode["Term", nextToken[tokens][[2]]],
    _, Message[parseTerm::unexpected, tok]; Abort[]
  ]
];
parseTerm::unexpected = "Unexpected token in term: `1`.";

parseList[tokens_] := Module[{head, tail},
  expect[tokens, "LBracket"];
  If[peekToken[tokens][[1]] === "RBracket",
    nextToken[tokens];
    ASTNode["List", {}],
    head = parseTerm[tokens];
    If[peekToken[tokens][[1]] === "Bar",
      nextToken[tokens];
      tail = parseTail[tokens];
      expect[tokens, "RBracket"];
      ASTNode["ListCons", head, tail],
      (* else regular list *)
      tail = {};
      While[peekToken[tokens][[1]] === "Comma",
        nextToken[tokens];
        AppendTo[tail, parseTerm[tokens]]
      ];
      expect[tokens, "RBracket"];
      ASTNode["List", Sequence @@ Prepend[tail, head]]
    ]
  ]
];

parseTail[tokens_] := Module[{tok = peekToken[tokens]},
  Switch[tok[[1]],
    "Variable", ASTNode["TailVar", nextToken[tokens][[2]]],
    "LBracket", parseList[tokens],
    "Bar", ASTNode["TailBar", nextToken[tokens][[2]]],
    _, Message[parseTail::unexpected, tok]; Abort[]
  ]
];
parseTail::unexpected = "Unexpected token in tail: `1`.";
