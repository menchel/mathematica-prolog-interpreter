ClearAll[
  tokenCreator,
  tokenizeFileLines,
  parser,
  parseASingleClause,
  parseASingleTerm,
  parseArguments,
  parseRuleBody,
  parseList,
  matchToken,
  Predicate
];


tokenCreator[inputCode_String] := Module[{tokens},

  tokens = StringCases[
    inputCode,
    {
      s : RegularExpression["\\s+"] :> Nothing,
      s : RegularExpression["[a-z][a-zA-Z0-9_]*"] :> {"Atom", s},
      s : RegularExpression["[A-Z_][a-zA-Z0-9_]*"] :> {"Variable", s},
      s : RegularExpression["\\d+(?:\\.\\d+)?"] :> {"Number", s},
      s : RegularExpression["'(?:[^']|'')*'"] :> {"String", s},
      
      ":-" :> {"ColonDash", ":-"},
      "." :> {"Dot", "."},
      "," :> {"Comma", ","},
      "(" :> {"LParen", "("},
      ")" :> {"RParen", ")"},
      "[" :> {"LBracket", "["},
      "]" :> {"RBracket", "]"},
      "|" :> {"Bar", "|"}
    }
  ];
  tokens
];

tokenizeFileLines[fileName_String] := Module[{lines, tokensPerLine},
  lines = Import[fileName, "Lines"];
  tokensPerLine = tokenCreator /@ lines;
  
  Do[
    Print["Line ", i, ": ", tokensPerLine[[i]]],
    {i, Length[lines]}
  ];
  
  tokensPerLine  (* Return tokens for all lines *)
];

ClearAll[parseASingleTerm, parseList, matchToken];

(* main parser function *)
parser[tokens_List]:= Module[{allClauses={},restOfTokens=tokens,singleClause,allRemain},
	While[restOfTokens!={}
		(* we need to parse each one *)
		{singleClause,allRemain} = parseASingleClause[restOfTokens];
		(* add to the parsed variables *)
		AppendTo[allClauses,singleClause];
		(* continue *)
		restOfTokens = allRemain
  		If[Length[restOfTokens] == 0, Break[]]; (* just in case *)
	];
	allClauses
];

(* a helper function to parse elements *)
parseASingleClause[tokens_List]:= Module[{start,body,next},
	(* first, we need to parse the first term *)
	{start,next} = parseASingleTerm[tokens];
	(* now we need a "switch case" to distinguish between options*)
	Which[
	next =!= {} && next[[1,1]] == "ColonDash", (* a wild rule has appeared! *)
	{body,next} = parseRuleBody[Rest[next]]
	matchToken["Dot",next];
	(* return the parsed version *)
	{Rule[start, body], Rest[next]},
	
	next =!= {} && next[[1,1]] == "Dot", (* a wild fact has appeared! *)
	{start, Rest[next]},
	
	True,
	(* fail *)
	Message[parseClause::bad, next]; Abort[]
	]
];

(* parse on a term level *)
parseASingleTerm[tokens_List]:=Module[{first, rest, arguments},
  (* parse the first element to start *)
  first = First[tokens];
  rest = Rest[tokens];
  (* let's look at the possibilities *)
  Which[
    first[[1]] === "Atom" || first[[1]] === "Variable", (* we saw an element we can't parse more *)
    If[rest =!= {} && rest[[1, 1]] === "LParen", (* it means that we start a fact *)
      {arguments, rest} = parseArguments[Rest[rest]]; (* get parsed arguments *)
      {Predicate[first[[2]], arguments], rest},
      {first[[2]], rest}
    ],
    
    (* a wild list has appeared *)
    first[[1]] === "LBracket",
    parseList[rest],
    
    (* another unparsable token *)
    first[[1]] === "Number" || first[[1]] === "String",
    {ToExpression[first[[2]]], rest},
    
   True, Message[parseTerm::bad, first]; Abort[]
  ]
];

(* parse arguments for the case fact(a,b,c...) *)
parseArguments[tokens_List] := Module[{arguements = {}, term, rest = tokens},
  (* as long as we can parse *)
  While[True,
    {term, rest} = parseASingleTerm[rest]; (* get element *)
    AppendTo[arguements, term];
    If[rest === {} || rest[[1, 1]] =!= "Comma", Break[]]; (* comma=continue, otherwise, end*)
    rest = Rest[rest];
  ];
  matchToken["RParen", rest]; (* if we ended, we must see an ) *)
  {arguements, Rest[rest]}
];

parseRuleBody[tokens_List] := Module[{terms = {}, term, rest = tokens},
 (* as long as we can parse *)
  While[True,
    {term, rest} = parseASingleTerm[rest]; (* get element *)
    AppendTo[terms, term];
    If[rest === {} || rest[[1, 1]] =!= "Comma", Break[]]; (* comma=continue, otherwise, end*)
    rest = Rest[rest];  (* end *)
  ];
  {terms, rest}
];

(* special case: parse a list *)
parseList[tokens_List] := Module[{elements = {}, head, rest = tokens, tail = "[]"},
  (* start list parse *)
  While[True,
    If[rest === {} || rest[[1, 1]] === "RBracket", Break[]]; (* end of list *)
    {head, rest} = parseTerm[rest]; (* get first term *)
    AppendTo[elements, head];
    
    Which[
      rest =!= {} && rest[[1, 1]] === "Comma", rest = Rest[rest], (* continue of list *)
      rest =!= {} && rest[[1, 1]] === "Bar", (* get only tail *)
        rest = Rest[rest];
        {tail, rest} = parseTerm[rest];
        Break[], (* it is tail, so it will end *)
      rest =!= {} && rest[[1, 1]] === "RBracket", Break[],
      True, Message[parseList::syntax, rest]; Abort[]
    ];
  ];
  matchToken["RBracket", rest]; (* end of list must be a ] *)
  {Fold[Predicate[".", {#2, #1}] &, tail, Reverse[elements]], Rest[rest]} (* continue *)
];

(* a helper function to the cases where a specific token is needed *)
matchToken[expected_String, tokens_List] := 
  If[tokens === {} || tokens[[1, 1]] =!= expected,
    Message[matchToken::expected, expected, tokens]; Abort[],
    tokens[[1]]
  ];
